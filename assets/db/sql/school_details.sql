CREATE TABLE school_details (
    school     NVARCHAR(100) NOT NULL,
    province   NVARCHAR(8)   NOT NULL,
    prefecture NVARCHAR(20), -- sometimes there is no perfecture
    county     NVARCHAR(20)  NOT NULL,
    PRIMARY KEY (school),
    FOREIGN KEY (school) REFERENCES users (school)
);
