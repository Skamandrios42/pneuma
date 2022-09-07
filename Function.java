import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public interface Function {
  public Object apply(Object arg);

  public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType type) throws NoSuchMethodException, IllegalAccessException {
    final MethodHandles.Lookup lookup = MethodHandles.lookup();
    // Need to use lookupClass() as this method is static
    final Class<?> currentClass = lookup.lookupClass();
    final MethodType targetSignature = MethodType.methodType(void.class);
    final MethodHandle targetMH = caller.findStatic(currentClass, "name", targetSignature);
    return new ConstantCallSite(targetMH.asType(type));
  }
}
