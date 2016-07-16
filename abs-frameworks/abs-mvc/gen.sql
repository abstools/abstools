CREATE TABLE TestImple (
name            VARCHAR(30)    DEFAULT 'test',
genId_my_prog        INT,
);

CREATE TABLE ProgramImpl (
tempat            VARCHAR(30)    DEFAULT '',
biaya            FLOAT        DEFAULT 0,
idProgram        INT        DEFAULT 0,
namaProgram        VARCHAR(30)    DEFAULT '',
penanggungJawab        VARCHAR(30)    DEFAULT '',
tanggal            VARCHAR(30)    DEFAULT '',
departemen        VARCHAR(30)    DEFAULT '',
peserta            INT        DEFAULT 0,
);

CREATE TABLE Program_TYPE (
genId                INT        NOT NULL,
PRIMARY KEY (genId),
);

ALTER TABLE TestImple
FOREIGN KEY genId_my_prog REFERENCES Program_TYPE(genId);



