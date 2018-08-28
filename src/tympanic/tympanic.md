

# Static checking

- imports exist on class path
- types mapped are visible (i.e., in the imported packages)
- types are not mapped multiples times
- constructor types are visible
- super types of constructor types are mapped (i.e., in types section)
- fields of Java constructor are present in corresponding class/interface (as field/nillary method)
- Java fields with constraints (on bool/int/enum/null) must have compatible types
- qualified enum references are visible in imports
- Java fields are not repeated (warning)
- Rascal constructor params are not repeated (i.e. unique)
- arity is compatible (modulo "skips")

# ADT generation

- extract ADT types from "types" section
- via super types of Java constructor types and mapping in "types" section, find out which Rascal constructors belong to which ADT (this is where M3 of JDT comes in)
- via types/super types of Java fields, map Rascal constructor params to the respective Rascal types as declared in the "types" section
- extract additional ADTs from "T x = f()" Rascal params
- generate the code according to the export directive

# Marshalling code generation

- generate Java visitor class with public static entry points for every type mapped in the "types" section
 
```
class Mapper {
  
  // for each mapped type
  public static IConstructor map(T t) {
    return new Mapper().visit(t);
  }
  
  // for each mapped type...
  IConstructor visit(T t) {
    if (t instanceof ConcreteType) {
      // recurse to kids
      return vf.constructor(...)
    }
    ...
    else {
      error
    }
  }
  
}
```

