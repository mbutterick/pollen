#lang pollen

◊(define inner 2)
◊(define edge (* inner 2))
◊(define color "gray")
◊(define multiplier 1.3)

body {
    margin: ◊|edge|em;
    border: ◊|inner|em double ◊|color|;
    padding: ◊|inner|em;
    font-size: ◊|multiplier|em;
    line-height: ◊|multiplier|;
}

h1 {
    font-size: ◊|multiplier|em;
}

#prev, #next {
position: fixed;
    top: ◊|(/ edge 2)|em;
}

#prev {
    left: ◊|edge|em;
}

#next {
    right: ◊|edge|em;
}