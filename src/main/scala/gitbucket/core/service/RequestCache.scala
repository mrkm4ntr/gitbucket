package gitbucket.core.service

import gitbucket.core.model.{Session, Issue, Account}
import gitbucket.core.util.Implicits
import gitbucket.core.controller.Context
import Implicits.request2Session
import gitbucket.core.view.LinkContext

/**
 * This service is used for a view helper mainly.
 *
 * It may be called many times in one request, so each method stores
 * its result into the cache which available during a request.
 */
trait RequestCache extends SystemSettingsService with AccountService with IssuesService {

  private implicit def context2Session(implicit context: Context): Session =
    request2Session(context.request)

  private implicit def linkContext2Session(implicit context: LinkContext): Session =
    context.session

  def getIssue(userName: String, repositoryName: String, issueId: String)
              (implicit context: Option[Context] = None, linkContext: Option[LinkContext] = None): Option[Issue] = {
    context.map { implicit context =>
      context.cache(s"issue.${userName}/${repositoryName}#${issueId}"){
      super.getIssue(userName, repositoryName, issueId)
      }
    }.getOrElse(linkContext.flatMap { implicit context =>
        super.getIssue(userName, repositoryName, issueId)
      })
  }

  def getCachedAccountByUserName(userName: String)
                          (implicit context: Option[Context] = None, linkContext: Option[LinkContext] = None): Option[Account] = {
    context.map { implicit context =>
      context.cache(s"account.${userName}"){
        super.getAccountByUserName(userName)
      }
    }.getOrElse(linkContext.flatMap { implicit context =>
      super.getAccountByUserName(userName)
    })
  }

  def getAccountByMailAddress(mailAddress: String)(implicit context: Context): Option[Account] = {
    context.cache(s"account.${mailAddress}"){
      super.getAccountByMailAddress(mailAddress)
    }
  }
}
