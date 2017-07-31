package edu.cmu.cs.vbc.prog.zipme;

import edu.cmu.cs.varex.annotation.VConditional;

public class FeatureSwitches {
    /*
     * DO NOT EDIT! THIS FILE IS AUTOGENERATED BY fstcomp
	 */

    @VConditional
    public static boolean __SELECTED_FEATURE_Base = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_CRC = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_ArchiveCheck = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_ZipMeTest = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_GZIP = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_Adler32Checksum = false;
    @VConditional
    public static boolean __SELECTED_FEATURE_Compress = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_Extract = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_DerivativeGZIPCRC = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_DerivativeCompressCRC = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_DerivativeExtractCRC = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_DerivativeCompressGZIP = true;
    @VConditional
    public static boolean __SELECTED_FEATURE_DerivativeCompressAdler32Checksum = false;
    @VConditional
    public static boolean __SELECTED_FEATURE_DerivativeCompressGZIPCRC = true;

    public static boolean __GUIDSL_ROOT_PRODUCTION = true;

    public static boolean valid() {
        return valid_product();
    }

    public static boolean valid_product() {
        if ((__SELECTED_FEATURE_Base)
                && (__SELECTED_FEATURE_ZipMeTest)
                && (__SELECTED_FEATURE_Compress)
                && (__SELECTED_FEATURE_Extract)
                && (!__SELECTED_FEATURE_GZIP || __SELECTED_FEATURE_CRC)
                && (__SELECTED_FEATURE_CRC || __SELECTED_FEATURE_Adler32Checksum)
                && (!(__SELECTED_FEATURE_Compress && __SELECTED_FEATURE_Adler32Checksum) ^ __SELECTED_FEATURE_DerivativeCompressAdler32Checksum)
                && (!(__SELECTED_FEATURE_Compress && __SELECTED_FEATURE_CRC) ^ __SELECTED_FEATURE_DerivativeCompressCRC)
                && (!(__SELECTED_FEATURE_Compress && __SELECTED_FEATURE_GZIP) ^ __SELECTED_FEATURE_DerivativeCompressGZIP)
                && (!(__SELECTED_FEATURE_Compress && __SELECTED_FEATURE_GZIP && __SELECTED_FEATURE_CRC) ^ __SELECTED_FEATURE_DerivativeCompressGZIPCRC)
                && (!(__SELECTED_FEATURE_Extract && __SELECTED_FEATURE_CRC) ^ __SELECTED_FEATURE_DerivativeExtractCRC)
                && (!(__SELECTED_FEATURE_GZIP && __SELECTED_FEATURE_CRC) ^ __SELECTED_FEATURE_DerivativeGZIPCRC))
            return true;
        else
            return false;
    }
}