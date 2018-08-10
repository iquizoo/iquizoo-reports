CREATE TABLE scores (
    userId     INT      NOT NULL,
    exerciseId INT      NOT NULL,
    createTime CHAR(19) NOT NULL,
    score      NUMERIC  NOT NULL,
    PRIMARY KEY (userId, exerciseId, createTime),
    FOREIGN KEY (userId) REFERENCES users (userId),
    FOREIGN KEY (exerciseId) REFERENCES exercises (exerciseId),
        CONSTRAINT score_id_positive
        CHECK (userId > 0 AND exerciseId > 0),
        CONSTRAINT score_create_time_chk
        CHECK (createTime LIKE '____-__-__ __:__:__') -- 'YYYY-mm-dd HH:MM:SS'
)
