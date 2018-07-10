CREATE TABLE class_covers (
    school NVARCHAR(100) NOT NULL,
    grade  INT           NOT NULL,
    class  NVARCHAR(10)  NOT NULL,
    cover  NVARCHAR(10)  NOT NULL,
    PRIMARY KEY (school, grade, class)
);
