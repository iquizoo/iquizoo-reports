CREATE TABLE school_covers (
    school NVARCHAR(100) NOT NULL,
    cover  NVARCHAR(100) NOT NULL,
    PRIMARY KEY (school),
    FOREIGN KEY (school) REFERENCES users (school)
);
