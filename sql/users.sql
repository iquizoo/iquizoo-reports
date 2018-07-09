CREATE TABLE users (
    userId INT          NOT NULL,
    name   VARCHAR(5),
    sex    VARCHAR(1),
    school VARCHAR(100),
    grade  INT,
    class  CHAR(10),
    PRIMARY KEY (userId),
        CONSTRAINT id_positive
        CHECK (userId > 0),
        CONSTRAINT sex_chinese
        CHECK (sex IN ('男', '女')),
        CONSTRAINT grade_range
        CHECK (grade BETWEEN 1 AND 9)
);
