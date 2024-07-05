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
     if sum > 10 then calculate := product else calculate := sum
  end;
  function add(x: integer; y: integer): integer;


  begin
    
     add := x + y;
  end;
begin
  
  calculate(2, 3);
  add(4, 5);
  calculate(2, 3);
  add(4, 5);
end.