import java.lang.invoke.*;

public class Target {
    public static void target() {
        System.out.println("Hello World!");
    }
        
    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType type) throws NoSuchMethodException, IllegalAccessException {
        final MethodHandles.Lookup lookup = MethodHandles.lookup();
        // Need to use lookupClass() as this method is static
        final Class<?> currentClass = lookup.lookupClass();
        final MethodType targetSignature = MethodType.methodType(void.class);
        final MethodHandle targetMH = lookup.findStatic(currentClass, "target", targetSignature);
        return new ConstantCallSite(targetMH.asType(type));
    }

}
