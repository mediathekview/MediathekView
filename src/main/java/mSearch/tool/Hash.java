package mSearch.tool;

import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public final class Hash {
    private final MessageDigest md;
    private byte[] bytes;

    public Hash() {
        try {
            md = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("MD5 algorithm not found", e);
        }
    }

    public Hash(byte[] b) {
        this();
        update(b);
    }

    public Hash(byte[] b, int off, int len) {
        this();
        update(b, off, len);
    }

    public Hash(ByteBuffer bb) {
        this();
        update(bb);
    }

    public Hash(String s) {
        this();
        update(s);
    }

    public Hash(char c) {
        this();
        update(c);
    }

    public Hash(short s) {
        this();
        update(s);
    }

    public Hash(int i) {
        this();
        update(i);
    }

    public Hash(long l) {
        this();
        update(l);
    }

    public Hash(float f) {
        this();
        update(f);
    }

    public Hash(double d) {
        this();
        update(d);
    }

    public Hash update(byte b) {
        checkNotFinished();
        md.update(b);
        return this;
    }

    public Hash update(byte[] b) {
        return update(b, 0, b.length);
    }

    public Hash update(byte[] b, int off, int len) {
        checkNotFinished();
        md.update(b, off, len);
        return this;
    }

    public Hash update(ByteBuffer bb) {
        checkNotFinished();
        md.update(bb);
        return this;
    }

    public Hash update(String s) {
        if (s != null) {
            for (int i = 0; i < s.length(); i++) {
                update(s.charAt(i));
            }
        }
        return this;
    }

    public Hash update(short s) {
        return update((byte) (s >> 8)).update((byte) s);
    }

    public Hash update(char c) {
        return update((short) c);
    }

    public Hash update(int i) {
        return update((short) (i >> 16)).update((short) i);
    }

    public Hash update(long l) {
        return update((int) (l >> 32)).update((int) l);
    }

    public Hash update(float f) {
        return update(Float.floatToRawIntBits(f));
    }

    public Hash update(double d) {
        return update(Double.doubleToRawLongBits(d));
    }

    public Hash finish() {
        bytes = md.digest();
        return this;
    }

    public void reset() {
        bytes = null;
    }

    public byte[] getBytes() {
        ensureFinished();
        return bytes;
    }

    public String toHexString() {
        ensureFinished();
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(Character.forDigit((b >> 8) & 0xf, 16));
            sb.append(Character.forDigit(b & 0xf, 16));
        }
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || !(obj instanceof Hash))
            return false;

        Hash hash = (Hash) obj;
        return Arrays.equals(getBytes(), hash.getBytes());
    }

    @Override
    public int hashCode() {
        byte[] b = getBytes();
        return (b[0] << 24) | (b[1] << 16) | (b[2] << 8) | b[3];
    }

    @Override
    public String toString() {
        return toHexString();
    }

    private void checkNotFinished() {
        if (bytes != null) {
            throw new IllegalStateException("Hash must be reset before resuse");
        }
    }

    private void ensureFinished() {
        if (bytes == null)
            finish();
    }
}
