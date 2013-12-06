alter table film add column bacon INT;

update film set bacon = 0 where film_id in (select af.film_id from actor_film af join actor a on a.actor_id = af.actor_id where a.bacon = 0);

update film set bacon = 1 where film_id in (select distinct af.film_id from actor_film af join actor a join film f on a.actor_id = af.actor_id and f.film_id = af.film_id where a.bacon = 1 and f.bacon is null);

update film set bacon = 2 where film_id in (select distinct af.film_id from actor_film af join actor a join film f on a.actor_id = af.actor_id and f.film_id = af.film_id where a.bacon = 2 and f.bacon is null);

