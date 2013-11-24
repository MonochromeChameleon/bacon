CREATE TABLE actor (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    imdbId VARCHAR(20),
    name VARCHAR(200) NOT NULL,
    baconNumber INT NOT NULL
);
CREATE INDEX ix_actor_imdbId ON actor(imdbId);
    
CREATE TABLE film (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    imdbId VARCHAR(20),
    name VARCHAR(200) NOT NULL,
    baconNumber INT NOT NULL
);
CREATE INDEX ix_film_imdbId ON film(imdbId);

CREATE TABLE actor_film (
    actor_id INTEGER NOT NULL,
    film_id INTEGER NOT NULL,
    FOREIGN KEY (actor_id) REFERENCES actor(id),
    FOREIGN KEY (film_id) REFERENCES film(id)
);