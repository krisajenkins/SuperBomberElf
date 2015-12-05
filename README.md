# Super Bomber Elf

A Super Bomberman clone. Written for [http://www.meetup.com/West-London-Hack-Night/](West London Hack Night).


## Building & Running

You'll need [stack](https://github.com/commercialhaskell/stack) and [Elm](http://elm-lang.org/).

(Note: If this is the first time you've run `stack`, it might take a
while to fetch & build the dependencies. Have coffee ready.)

### Frontend

``` sh
cd client
stack runhaskell Shake.hs
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
