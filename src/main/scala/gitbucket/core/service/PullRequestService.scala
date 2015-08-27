package gitbucket.core.service

import gitbucket.core.model.Profile._
import gitbucket.core.model.{Account, Issue, PullRequest}
import gitbucket.core.util.ControlUtil._
import gitbucket.core.util.Directory._
import gitbucket.core.util.JGitUtil
import gitbucket.core.util.JGitUtil.CommitInfo
import org.eclipse.jgit.api.Git
import profile.simple._

import scala.collection.JavaConverters._


trait PullRequestService { self: IssuesService with ActivityService with RepositoryService =>
  import PullRequestService._

  def getPullRequest(owner: String, repository: String, issueId: Int)
                    (implicit s: Session): Option[(Issue, PullRequest)] =
    getIssue(owner, repository, issueId.toString).flatMap{ issue =>
      PullRequests.filter(_.byPrimaryKey(owner, repository, issueId)).firstOption.map{
        pullreq => (issue, pullreq)
      }
    }

  def updateCommitId(owner: String, repository: String, issueId: Int, commitIdTo: String, commitIdFrom: String)
                    (implicit s: Session): Unit =
    PullRequests.filter(_.byPrimaryKey(owner, repository, issueId))
                .map(pr => pr.commitIdTo -> pr.commitIdFrom)
                .update((commitIdTo, commitIdFrom))

  def getPullRequestCountGroupByUser(closed: Boolean, owner: Option[String], repository: Option[String])
                                    (implicit s: Session): List[PullRequestCount] =
    PullRequests
      .innerJoin(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
        (t2.closed         === closed.bind) &&
        (t1.userName       === owner.get.bind, owner.isDefined) &&
        (t1.repositoryName === repository.get.bind, repository.isDefined)
      }
      .groupBy { case (t1, t2) => t2.openedUserName }
      .map { case (userName, t) => userName -> t.length }
      .sortBy(_._2 desc)
      .list
      .map { x => PullRequestCount(x._1, x._2) }

//  def getAllPullRequestCountGroupByUser(closed: Boolean, userName: String)(implicit s: Session): List[PullRequestCount] =
//    PullRequests
//      .innerJoin(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
//      .innerJoin(Repositories).on { case ((t1, t2), t3) => t2.byRepository(t3.userName, t3.repositoryName) }
//      .filter { case ((t1, t2), t3) =>
//        (t2.closed === closed.bind) &&
//          (
//            (t3.isPrivate === false.bind) ||
//            (t3.userName  === userName.bind) ||
//            (Collaborators.filter { t4 => t4.byRepository(t3.userName, t3.repositoryName) && (t4.collaboratorName === userName.bind)} exists)
//          )
//      }
//      .groupBy { case ((t1, t2), t3) => t2.openedUserName }
//      .map { case (userName, t) => userName -> t.length }
//      .sortBy(_._2 desc)
//      .list
//      .map { x => PullRequestCount(x._1, x._2) }

  def createPullRequest(originUserName: String, originRepositoryName: String, issueId: Int,
        originBranch: String, requestUserName: String, requestRepositoryName: String, requestBranch: String,
        commitIdFrom: String, commitIdTo: String)(implicit s: Session): Unit =
    PullRequests insert PullRequest(
      originUserName,
      originRepositoryName,
      issueId,
      originBranch,
      requestUserName,
      requestRepositoryName,
      requestBranch,
      commitIdFrom,
      commitIdTo)

  def getPullRequestsByRequest(userName: String, repositoryName: String, branch: String, closed: Boolean)
                              (implicit s: Session): List[PullRequest] =
    PullRequests
      .innerJoin(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
        (t1.requestUserName       === userName.bind) &&
        (t1.requestRepositoryName === repositoryName.bind) &&
        (t1.requestBranch         === branch.bind) &&
        (t2.closed                === closed.bind)
      }
      .map { case (t1, t2) => t1 }
      .list

  def getPullRequestsByTarget(userName: String, repositoryName: String, branch: String, closed: Boolean)
                              (implicit s: Session): List[PullRequest] =
    PullRequests
      .innerJoin(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
      (t1.requestUserName       === userName.bind) &&
        (t1.requestRepositoryName === repositoryName.bind) &&
        (t1.branch                === branch.bind) &&
        (t2.closed                === closed.bind)
    }
      .map { case (t1, t2) => t1 }
      .list

  /**
   * for repository viewer.
   * 1. find pull request from from `branch` to othre branch on same repository
   *   1. return if exists pull request to `defaultBranch`
   *   2. return if exists pull request to othre branch
   * 2. return None
   */
  def getPullRequestFromBranch(userName: String, repositoryName: String, branch: String, defaultBranch: String)
                              (implicit s: Session): Option[(PullRequest, Issue)] =
    PullRequests
      .innerJoin(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
        (t1.requestUserName       === userName.bind) &&
        (t1.requestRepositoryName === repositoryName.bind) &&
        (t1.requestBranch         === branch.bind) &&
        (t1.userName              === userName.bind) &&
        (t1.repositoryName        === repositoryName.bind) &&
        (t2.closed                === false.bind)
      }
      .sortBy{ case (t1, t2) => t1.branch =!= defaultBranch.bind }
      .firstOption

  /**
   * Fetch pull request contents into refs/pull/${issueId}/head and update pull request table.
   */
  def updatePullRequests(owner: String, repository: String, branch: String)(implicit s: Session): Unit =
    getPullRequestsByRequest(owner, repository, branch, false).foreach { pullreq =>
      if(Repositories.filter(_.byRepository(pullreq.userName, pullreq.repositoryName)).exists.run){
        val (commitIdTo, commitIdFrom) = JGitUtil.updatePullRequest(
          pullreq.userName, pullreq.repositoryName, pullreq.branch, pullreq.issueId,
          pullreq.requestUserName, pullreq.requestRepositoryName, pullreq.requestBranch)
        updateCommitId(pullreq.userName, pullreq.repositoryName, pullreq.issueId, commitIdTo, commitIdFrom)
      }
    }

  /**
   * Close merged pull request and return issue to notify.
   */
  def closeMergedPullRequest(repository: RepositoryService.RepositoryInfo, issueId: Int, message: String,
        committer: Account, baseUrl: String)(implicit session: Session): Option[Issue] = {
    val owner = repository.owner
    val name = repository.name
    getPullRequest(owner, name, issueId).map { case (issue, pullreq) =>
      using(Git.open(getRepositoryDir(owner, name))) { git =>
        // mark issue as merged and close.
        createComment(owner, name, committer.userName, issueId, message, "merge")
        createComment(owner, name, committer.userName, issueId, "Close", "close")
        updateClosed(owner, name, issueId, true)

        // record activity
        recordMergeActivity(owner, name, committer.userName, issueId, message)

        val commits = using(
          Git.open(getRepositoryDir(owner, name)),
          Git.open(getRepositoryDir(pullreq.requestUserName, pullreq.repositoryName))
        ){ (oldGit, newGit) =>
          val oldId = oldGit.getRepository.resolve(pullreq.commitIdFrom)
          val newId = newGit.getRepository.resolve(pullreq.commitIdTo)

          newGit.log.addRange(oldId, newId).call.iterator.asScala.map { revCommit =>
            new CommitInfo(revCommit)
          }.toList
        }

        // close issue by content of pull request
        val defaultBranch = getRepository(owner, name, baseUrl).get.repository.defaultBranch
        if(pullreq.branch == defaultBranch){
          commits.foreach { commit =>
            closeIssuesFromMessage(commit.fullMessage, committer.userName, owner, name)
          }
          issue.content match {
            case Some(content) => closeIssuesFromMessage(content, committer.userName, owner, name)
            case _ =>
          }
          closeIssuesFromMessage(message, committer.userName, owner, name)
        }
        // call web hook
        //callPullRequestWebHook("closed", repository, issueId, baseUrl, committer)

        // notifications
        /*Notifier().toNotify(repository, issue, "merge"){
          Notifier.msgStatus(s"${baseUrl}/${owner}/${name}/pull/${issueId}")
        }*/

        issue
      }
    }
  }

  def getPullRequestByRequestCommit(userName: String, repositoryName: String, toBranch:String, fromBranch: String, commitId: String)
                              (implicit s: Session): Option[(PullRequest, Issue)] = {
    if(toBranch == fromBranch){
      None
    } else {
      PullRequests
        .innerJoin(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
        .filter { case (t1, t2) =>
          (t1.userName              === userName.bind) &&
          (t1.repositoryName        === repositoryName.bind) &&
          (t1.branch                === toBranch.bind) &&
          (t1.requestUserName       === userName.bind) &&
          (t1.requestRepositoryName === repositoryName.bind) &&
          (t1.requestBranch         === fromBranch.bind) &&
          (t1.commitIdTo            === commitId.bind)
        }
        .firstOption
    }
  }
}

object PullRequestService {

  val PullRequestLimit = 25

  case class PullRequestCount(userName: String, count: Int)

}
