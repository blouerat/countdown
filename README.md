# Countdown

Parser for the number part of the game [Countdown](https://en.wikipedia.org/wiki/Countdown_(game_show)).
You can learn more about the rules [here](http://www.datagenetics.com/blog/august32014/index.html).

### Build

```
> idris --build countdown.ipkg
Type checking ./Parsers.idr
Type checking ./Main.idr
```

### Run

```
> ./countdown
Enter a valid countdown number expression: 50 * 10 + 8 * 7
Nice one: 50 * 10 + 8 * 7 = 556
Enter a valid countdown number expression: ((50 - 7) * 3 + 10) * 8 / 2
Nice one: ((50 - 7) * 3 + 10) * 8 / 2 = 556
Enter a valid countdown number expression: (((((  25  -4)))))*   2
Nice one: (25 - 4) * 2 = 42
Enter a valid countdown number expression: ()
Nope!
()
 ^ Unexpected token: ), expected a number or an opening bracket
Enter a valid countdown number expression: 1 +
Nope!
1 +
   ^ Unexpected EOF, expected a number or an opening bracket
Enter a valid countdown number expression: 72
Nope!
72
^ Invalid number 72, expected one of: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 25, 50, 75, 100]
Enter a valid countdown number expression: 75
Nice one: 75 = 75
Enter a valid countdown number expression: ^C
```
