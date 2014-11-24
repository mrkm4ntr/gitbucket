CREATE TABLE COMMIT_COMMENT (
    USER_NAME VARCHAR(100) NOT NULL,
    REPOSITORY_NAME VARCHAR(100) NOT NULL,
    COMMIT_ID VARCHAR(100) NOT NULL,
    COMMENT_ID INT AUTO_INCREMENT,
    COMMENTED_USER_NAME VARCHAR(100) NOT NULL,
    CONTENT TEXT NOT NULL,
    FILE_NAME NVARCHAR(100),
    OLD_LINE_NUMBER INT,
    NEW_LINE_NUMBER INT,
    REGISTERED_DATE TIMESTAMP NOT NULL,
    UPDATED_DATE TIMESTAMP NOT NULL
);

ALTER TABLE COMMIT_COMMENT ADD CONSTRAINT IDX_COMMIT_COMMENT_PK PRIMARY KEY (COMMENT_ID);
ALTER TABLE COMMIT_COMMENT ADD CONSTRAINT IDX_COMMIT_COMMENT_1 UNIQUE (USER_NAME, REPOSITORY_NAME, COMMIT_ID, COMMENT_ID);
