CREATE TABLE exercises (
    exerciseId INT NOT NULL,
    name VARCHAR(100), -- exercise name
    title VARCHAR(100), -- exercise title displayed
    PRIMARY KEY (exerciseId),
        CONSTRAINT exercise_id_positive
        CHECK (exerciseId > 0)
);
