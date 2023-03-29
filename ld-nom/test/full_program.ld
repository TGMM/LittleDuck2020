program my_program;
var a, x, y, z: int;
var f: float;
{
    a = 10 + 10;
    a = 10 > 10;
    a = 10 < 10;
    a = 10 <> 10;
    x = 10 + 5 * 30;
    y = (10 + 5) * 30;
    z = 10 + (5 * 30);
    a = y;

    print("x: ", x, "\n", "z: ", z, "\n");
    if(x > z) {
        print("Compiladores me la succ");
    } else {
        print("Yep compiler ", a, " ", 10);
    };
}