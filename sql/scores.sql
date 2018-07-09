CREATE TABLE scores (
    userId     INT         NOT NULL,
    exerciseId INT         NOT NULL,
    createTime VARCHAR(30) NOT NULL,
    score      NUMERIC     NOT NULL,
    PRIMARY KEY (userId, exerciseId, createTime),
    FOREIGN KEY (exerciseId) REFERENCES exercises (exerciseId),
        CONSTRAINT score_id_positive
        CHECK (userId > 0 AND exerciseId > 0),
        CONSTRAINT score_range
        CHECK (score BETWEEN 50 AND 150)
)
