program Yamil;
var
  variable : integer = 2;
  sum : integer = a + b;
  function calculate(num1: integer; num2: integer): integer;
  var
    sum: integer;
    product: integer;
  begin
    sum := num1 + num2;
    product := num1 * num2;
     if sum > 10 
    then product else sum
  end;
  function add(x: integer; y: integer): integer;


  begin
    
     add := x + y;
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
  
  calculate(2, 3);
  add(4, 5);
  match_digit(1);
  match_digit(2);
  match_digit(3);
  match_letters('Hey');
  match_letters('C');
  match_letters('Haskell');
  calculate(2, 3);
  add(4, 5);
  match_digit(1);
  match_digit(2);
  match_digit(3);
  match_letters('Hey');
  match_letters('C');
  match_letters('Haskell');
end.