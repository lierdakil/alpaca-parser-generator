<?php
namespace Alpaca;

require './Lexer.php';

$debug = $argv[1] == '-d';
while($f = fgets(STDIN)){
    $lexer = new Lexer(rtrim($f), $debug);
    while(($token = $lexer->getNextToken())[0] !== Lexer::TOKEN_TYPE_EOF) {
      echo(Lexer::tokenToString($token[0]) . ", " . $token[1] . "\n");
    }
}
