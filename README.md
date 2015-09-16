# Rally Exercise - The Game of Life
## Usage

If you have clojure and Leiningen installed, feel free to simply `git clone` this repo and `lein run`
```bash
$ git clone git@github.com:alexfaber2011/rally-exercises.git
$ cd rally-exercises
$ lein run
```

If you'd rather not go through the trouble of installing clojure and leiningen, then here's a link to the jar this project compiles to: https://www.dropbox.com/s/vj94gfgehuu8o2a/exercises-0.1.0-SNAPSHOT-standalone.jar?dl=0.
```bash
$ java -jar exercises-0.1.0-SNAPSHOT-standalone.jar
````
## Notes
My implementation assumes that the input is valid (consisting of only 0's and 1's and newlines).

An inbalanced board, one that isn't rectangular, won't break the game, but it may lead to unexpected results

Have fun!

-Alex
