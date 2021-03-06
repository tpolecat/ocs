;              
CREATE USER IF NOT EXISTS "" SALT '' HASH '' ADMIN;            
CREATE CACHED TABLE PUBLIC.VERSION(
    VALUE INTEGER NOT NULL
);              
-- 1 +/- SELECT COUNT(*) FROM PUBLIC.VERSION;  
INSERT INTO PUBLIC.VERSION(VALUE) VALUES
(1);  
CREATE CACHED TABLE PUBLIC.KEYS(
    CLASS VARCHAR NOT NULL,
    NAME VARCHAR NOT NULL,
    HASH VARCHAR NOT NULL,
    VERSION INTEGER DEFAULT 1 NOT NULL
);   
-- 5 +/- SELECT COUNT(*) FROM PUBLIC.KEYS;     
INSERT INTO PUBLIC.KEYS(CLASS, NAME, HASH, VERSION) VALUES
('User', 'bob@dobbs.com', 'foodle', 1),
('Program', 'GS-2010A-Q-3', 'blippy', 1),
('Staff', 'Gemini', 'hoox', 2),
('Affiliate', 'Chile', 'huevon', 123),
('Visitor', 'GN-2010-C-3', 'doop', 1);        
CREATE UNIQUE INDEX PUBLIC.KEY_IDX ON PUBLIC.KEYS(CLASS, NAME);
