package edu.cmu.cs.vbc;

import org.objectweb.asm.*;

import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;

public class Main extends ClassLoader{


    public byte[] generator(String caller, String callee) throws ClassNotFoundException{
        String resource = callee.replace('.', '/') + ".class";
        InputStream is =  getResourceAsStream(resource);
        byte[] buffer;
        // adapts the class on the fly
        try {
            ClassReader cr = new ClassReader(is);
//            ClassNode classNode = new ClassNode();
//            cr.accept(classNode, 0);

            resource = caller.replace('.', '/')+".class";
            is = getResourceAsStream(resource);
            cr = new ClassReader(is);
            ClassWriter cw = new ClassWriter(0);
//            ClassVisitor visitor = new MergeAdapter(cw, classNode);
//            cr.accept(visitor, 0);

            buffer= cw.toByteArray();

        } catch (Exception e) {
            throw new ClassNotFoundException(caller, e);
        }

        // optional: stores the adapted class on disk
        try {
            FileOutputStream fos = new FileOutputStream("/tmp/data.adapted");
            fos.write(buffer);
            fos.close();
        } catch (IOException e) {}
        return buffer;
    }


    @Override
    protected synchronized Class<?> loadClass(final String name,
                                              final boolean resolve) throws ClassNotFoundException {
        if (name.startsWith("java.")) {
            System.err.println("Adapt: loading class '" + name
                    + "' without on the fly adaptation");
            return super.loadClass(name, resolve);
        }

            System.err.println("Adapt: loading class '" + name
                    + "' with on the fly adaptation");
//        }
//        byte[] b = generator(caller, callee);

        try {
            String resource = name.replace('.', '/') + ".class";
            InputStream is = getResourceAsStream(resource);
            ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
            int nextValue = is.read();
            while (-1 != nextValue) {
                byteStream.write(nextValue);
                nextValue = is.read();
            }

            byte[] classByte = byteStream.toByteArray();
            return defineClass(name, classByte, 0, classByte.length, null);

        } catch (IOException e) {
            throw new ClassNotFoundException();
        }
    }

    public static void main(final String args[]) throws Exception {
        // loads the application class (in args[0]) with an Adapt class loader
        ClassLoader loader = new Main();
        Class<?> c = loader.loadClass("edu.cmu.cs.vbc.prog.Main");
        Method m = c.getMethod("main", new Class<?>[] { String[].class });
        String[] applicationArgs = new String[0];
//        System.arraycopy(args, 1, applicationArgs, 0, applicationArgs.length);
        m.invoke(null, new Object[] { applicationArgs });
    }
}