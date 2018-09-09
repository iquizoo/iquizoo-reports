CREATE TABLE users (
    userId INT          NOT NULL,
    name   NVARCHAR(5),
    sex    NCHAR(1),
    school NVARCHAR(100),
    grade  INT,
    class  NVARCHAR(10),
    PRIMARY KEY (userId),
        CONSTRAINT user_id_positive
        CHECK (userId > 0),
        CONSTRAINT sex_chinese
        CHECK (sex IN ('男', '女')),
        CONSTRAINT grade_range
        CHECK (grade BETWEEN 1 AND 9)
);
