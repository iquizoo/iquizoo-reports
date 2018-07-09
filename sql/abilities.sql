CREATE TABLE abilities (
    abId    INT         NOT NULL,
    parent  INT         NOT NULL, -- parent ability, use 0 if there is no parent
    name    VARCHAR(10), -- ability name
    name_en VARCHAR(30), -- ability english name
    PRIMARY KEY (abId),
        CONSTRAINT ability_id_positive
        CHECK (abId > 0)
);
