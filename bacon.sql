CREATE TABLE actor (
    imdbId VARCHAR(20) PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    baconNumber INT NOT NULL,
    processed BOOL NOT NULL DEFAULT FALSE
);
    
CREATE TABLE film (
    imdbId VARCHAR(20) PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    baconNumber INT NOT NULL,
    processed BOOL NOT NULL DEFAULT FALSE
);

CREATE TABLE actor_film (
    actor_id VARCHAR(20) NOT NULL,
    film_id VARCHAR(20) NOT NULL,
    FOREIGN KEY (actor_id) REFERENCES actor(imdbId),
    FOREIGN KEY (film_id) REFERENCES film(imdbId)
);