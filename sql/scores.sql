CREATE TABLE scores (
    PRIMARY KEY (userId, exerciseId, createTime),
    FOREIGN KEY (exerciseId) REFERENCES exercises (exerciseId),
    userId     INT         NOT NULL,
    exerciseId INT         NOT NULL,
    createTime VARCHAR(30) NOT NULL,
    score      NUMERIC     NOT NULL,
               CONSTRAINT chk_id
               CHECK (userId > 0, exerciseId > 0),
               CONSTRAINT score_range
               CHECK (score BETWEEN 50 AND 150)
)
