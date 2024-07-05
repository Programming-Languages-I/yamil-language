program Yamil;
var
  num1: integer;
  num2: integer;
  x: integer;
  y: integer;
  a: integer;
  b: integer;
  c: integer;
  p: integer;
  l: string;
  
begin
  function calculate(num1: integer;, num2: integer;): integer;
  begin
    sum : integer = num1 + num2;
    product : integer = num1 * num2;
    if sum > 10 then product else sum
  end;
  calculate(2, 3);
  function add(x: integer;, y: integer;): integer;
  begin
    x + y

  end;
  add(4, 5);
  function max_of(a: integer;, b: integer;, c: integer;): integer;
  begin
    if a > b then if a > c then a else c else if b > c then b else c
  end;
  max_of(3, 7, 5);
  max_of(10, 3, 5);
  max_of(1, 2, 3);
  function isPositive(): boolean;
  begin
    procedure operation(t: integer); begin   if t > 0 then True else False end;
  end;
  isPositive(2);
  max_of(10, 15, 1);
  max_of(10, 5, 3);
  function match_digit(p: integer;): string;
  begin
    case x of
      p: WriteLn("Zero");
      1: WriteLn("One");
      2: WriteLn("Two");
      3: WriteLn("Three");
      4: WriteLn("Four");
      5: WriteLn("Five");
      6: WriteLn("Six");
      7: WriteLn("Seven");
      8: WriteLn("Eight");
      9: WriteLn("Nine");
      10: WriteLn("Ten");
      else WriteLn("Not Found");
    end;
  end;
  match_digit(1);
  match_digit(2);
  match_digit(3);
  function match_letters(l: string;): boolean;
  begin
    case x of
      l: WriteLn(True);
      "Haskell": WriteLn(True);
      "Java": WriteLn(False);
      "C": WriteLn(True);
      "Go": WriteLn(False);
      else WriteLn(False);
    end;
  end;
  match_letters("Hey");
  match_letters("C");
  match_letters("Haskell");
end.