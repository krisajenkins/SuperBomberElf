# Super Bomber Elf

A Super Bomberman clone. Written for [http://www.meetup.com/West-London-Hack-Night/](West London Hack Night).

![Super Bomber Elf](screenshot.png)

## Building & Running

You'll need [stack](https://github.com/commercialhaskell/stack) and [Elm 0.18](http://elm-lang.org/).

(Note: If this is the first time you've run `stack`, it might take a
while to fetch & build the dependencies. Have coffee ready.)

### Frontend

``` sh
cd client
make -w
```

### Backend

Copy `server/bomberman.sample.yaml` to `~/.bomberman.yaml` and edit appropriately. Then:

``` sh
cd server
stack build
stack exec bomberman
```

### Testing

``` sh
cd server
stack test
```
