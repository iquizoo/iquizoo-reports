CREATE TABLE abilities (
    PRIMARY KEY (abId),
    abId    INT         NOT NULL,
    parent  INT         NOT NULL, -- parent ability, use 0 if there is no parent
    name    VARCHAR(10), -- ability name
    name_en VARCHAR(30), -- ability english name
            CONSTRAINT chk_abId
            CHECK (abId > 0)
);
