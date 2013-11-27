CREATE TABLE actor (
    actor_id VARCHAR(20) PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    baconNumber INT NOT NULL,
    processed BOOL NOT NULL DEFAULT False
);
    
CREATE TABLE film (
    film_id VARCHAR(20) PRIMARY KEY,
    title VARCHAR(200) NOT NULL,
    year INT NOT NULL,
    processed BOOL NOT NULL DEFAULT False
);

CREATE TABLE actor_film (
    actor_id VARCHAR(20) NOT NULL,
    film_id VARCHAR(20) NOT NULL,
    FOREIGN KEY (actor_id) REFERENCES actor(actor_id),
    FOREIGN KEY (film_id) REFERENCES film(film_id)
);