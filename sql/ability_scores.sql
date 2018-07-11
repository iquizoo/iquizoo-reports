CREATE TABLE ability_scores (
    userId INT     NOT NULL,
    abId   INT     NOT NULL,
    score  NUMERIC NOT NULL,
    PRIMARY KEY (userId, abId),
    FOREIGN KEY (userId) REFERENCES users (userId),
    FOREIGN KEY (abId) REFERENCES abilities (abId),
        CONSTRAINT ability_score_id_positive
        CHECK (userId > 0 AND abId > 0)
);
