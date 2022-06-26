/**
 * Converts a PrintStream, which is a subclass of OutputStream, but which doesn't
 * throw IOExceptions, back into an OutputStream which does throw IOExceptions.
 *
 * @author Jacob Maine
 *
 */

package clojure_lsp.helpers;

import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.PrintStream;

public class OutputPrintStream extends FilterOutputStream {
    private final PrintStream out;

    public OutputPrintStream(PrintStream out) {
        super(new ByteArrayOutputStream(0));
        this.out = out;
    }

    @Override
    public void close() throws IOException {
        out.close();
        throwOnError();
    }

    @Override
    public void flush() throws IOException {
        out.flush();
        throwOnError();
    }

    @Override
    public void write(int b) throws IOException {
        out.write(b);
        throwOnError();
    }

    @Override
    public void write(byte[] buf, int off, int len) throws IOException {
        out.write(buf, off, len);
        throwOnError();
    }

    @Override
    public void write(byte[] b) throws IOException {
        out.write(b);
        throwOnError();
    }

    private void throwOnError() throws IOException {
        if (out.checkError()) {
            // We don't know that the stream closed. Something else might have
            // happened. But this is what lsp4j expects to see.
            throw new IOException("Stream closed");
        }
    }
}
