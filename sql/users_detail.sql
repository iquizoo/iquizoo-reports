CREATE TABLE users_detail (
    userId       INT            NOT NULL,
    school       NVARCHAR(100),
    grade        INT,
    class        NVARCHAR(10),
    PRIMARY KEY (userId),
    FOREIGN KEY (userId) REFERENCES users (userId) ON DELETE CASCADE,
        CONSTRAINT user_detail_id_positive
        CHECK (userId > 0),
        CONSTRAINT grade_range
        CHECK (grade BETWEEN 1 AND 9)
)
