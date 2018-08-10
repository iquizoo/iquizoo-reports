CREATE TABLE exercise_ability (
    exerciseId INT NOT NULL,
    abId       INT NOT NULL,
    PRIMARY KEY (exerciseId, abId),
    FOREIGN KEY (exerciseId) REFERENCES exercises,
    FOREIGN KEY (abId) REFERENCES abilities,
        CONSTRAINT ex_ab_id_positive
        CHECK (exerciseId > 0 AND abId > 0)
)
