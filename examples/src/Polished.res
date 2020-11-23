@module("polished") external invert: string => string = "invert"
let invert = invert // it must be bound so compiler generates a function in this module accesible from JS

@module("polished") external lighten: (float, string) => string = "lighten"
let lighten = (color, amount) => lighten(amount, color) // so we can pipe color
