program Yamil;
var
  variable : integer = 2;
  function calculate(num1: integer; num2: integer): integer;
  var
    sum: integer;
    product: integer;

  begin
    sum := num1 + num2;
    product := num1 * num2;
     if sum > 10 
    then calculate := product else calculate := sum
  end;
  function isPositive(): boolean;


  procedure operation(t: integer); begin   if t > 0 
                                           then isPositive := True else isPositive := False end;
  begin
    writeln(operation(2));
  end;
  function add(x: integer; y: integer): integer;



  begin
    
     add := x + y;
  end;
  function max_of(a: integer; b: integer; c: integer): integer;



  begin
     if a > b 
    then if a > c 
    then max_of := a else max_of := c else if b > c 
    then max_of := b else max_of := c
  end;
  function match_digit(p: integer): string;



  begin
    case p of
      1: match_digit :='One';
      2: match_digit :='Two';
      3: match_digit :='Three';
      4: match_digit :='Four';
      5: match_digit :='Five';
      6: match_digit :='Six';
      7: match_digit :='Seven';
      8: match_digit :='Eight';
      9: match_digit :='Nine';
      10: match_digit :='Ten';
      else WriteLn('Not Found');
    end;
  end;
  function match_letters(p: string): boolean;



  begin
    case p of
      'Haskell': match_letters :=True;
      'Java': match_letters :=False;
      'C': match_letters :=True;
      'Go': match_letters :=False;
      else WriteLn(False);
    end;
  end;
begin
  
  writeln(calculate(2, 3));
  writeln(isPositive());
  writeln(add(4, 5));
  writeln(max_of(3, 7, 5));
  writeln(max_of(10, 3, 5));
  writeln(max_of(1, 2, 3));
  writeln(match_digit(1));
  writeln(match_digit(2));
  writeln(match_digit(3));
  writeln(match_letters('Hey'));
  writeln(match_letters('C'));
  writeln(match_letters('Haskell'));
  writeln(calculate(2, 3));
  writeln(isPositive());
  writeln(add(4, 5));
  writeln(max_of(3, 7, 5));
  writeln(max_of(10, 3, 5));
  writeln(max_of(1, 2, 3));
  writeln(match_digit(1));
  writeln(match_digit(2));
  writeln(match_digit(3));
  writeln(match_letters('Hey'));
  writeln(match_letters('C'));
  writeln(match_letters('Haskell'));
end.