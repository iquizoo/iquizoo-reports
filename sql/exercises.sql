CREATE TABLE exercises (
    exerciseId INT            NOT NULL,
    name       NVARCHAR(100), -- exercise name
    title      NVARCHAR(100), -- exercise title displayed
    PRIMARY KEY (exerciseId),
        CONSTRAINT exercise_id_positive
        CHECK (exerciseId > 0)
);
