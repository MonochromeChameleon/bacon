CREATE TABLE actor (
    actor_id VARCHAR(20) PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    bacon INT NOT NULL,
    processed BOOL NOT NULL DEFAULT False
);
    
CREATE TABLE film (
    film_id VARCHAR(20) PRIMARY KEY,
    title VARCHAR(200) NOT NULL,
    year INT NOT NULL,
    bacon INT NOT NULL,
    processed BOOL NOT NULL DEFAULT False
);

CREATE TABLE actor_film (
    actor_id VARCHAR(20) NOT NULL,
    film_id VARCHAR(20) NOT NULL,
    FOREIGN KEY (actor_id) REFERENCES actor(actor_id),
    FOREIGN KEY (film_id) REFERENCES film(film_id)
);

create index ix_a_id ON actor (actor_id);
create index ix_f_id ON film (film_id);
create index ix_af_a_id ON actor_film (actor_id);
create index ix_af_f_id ON actor_film (film_id);