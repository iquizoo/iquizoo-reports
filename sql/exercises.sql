CREATE TABLE exercises (
    exerciseId INT NOT NULL,
    name VARCHAR(100), -- exercise name
    title VARCHAR(100), -- exercise title displayed
        CONSTRAINT chk_id
        CHECK (exerciseId > 0),
        PRIMARY KEY (exerciseId)
);
