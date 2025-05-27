(ns com.example.anpr.core
    "ANPR Clojure Example
     This example demonstrates how to use the TSANPR engine via JNI in Clojure."
    (:gen-class)
    (:import [java.nio.file Files Paths]
             [java.nio.charset StandardCharsets]
             [java.io File PrintStream IOException]
             [javax.imageio ImageIO]
             [java.awt.image BufferedImage DataBufferByte DataBufferInt]
             [com.example.anpr TSANPR]))

;; The MIT License (MIT)
;; Copyright © 2022-2025 TS-Solution Corp.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to all conditions.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(def examples-base-dir "../..")

(defn get-jni-file-name
  "Generate JNI wrapper filename depending on platform."
  []
  (let [os   (.. System (getProperty "os.name") toLowerCase)
        arch (.. System (getProperty "os.arch") toLowerCase)
        cwd  "."]
    (cond
     (.contains os "win")
     (if (.contains arch "64")
       (str cwd "/bin/windows-x86_64/jni/Debug/tsanpr_jni.dll")
       (str cwd "/bin/windows-x86/jni/Debug/tsanpr_jni.dll"))
     (.contains os "linux")
     (cond
      (.contains arch "aarch64")
      (str cwd "/bin/linux-aarch64/jni/libtsanpr_jni.so")
      (.contains arch "64")
      (str cwd "/bin/linux-x86_64/jni/libtsanpr_jni.so")
      :else (throw (RuntimeException. (str "Unsupported OS/arch: " os "/" arch))))
     :else (throw (RuntimeException. (str "Unsupported OS/arch: " os "/" arch))))))

(defn get-engine-file-name
  "Generate engine filename depending on platform."
  []
  (let [os   (.. System (getProperty "os.name") toLowerCase)
        arch (.. System (getProperty "os.arch") toLowerCase)]
    (cond
     (.contains os "win")
     (if (.contains arch "64")
       (str examples-base-dir "/bin/windows-x86_64/tsanpr.dll")
       (str examples-base-dir "/bin/windows-x86/tsanpr.dll"))
     (.contains os "linux")
     (cond
      (.contains arch "aarch64")
      (str examples-base-dir "/bin/linux-aarch64/jni/libtsanpr_jni.so")
      (.contains arch "64")
      (str examples-base-dir "/bin/linux-x86_64/jni/libtsanpr_jni.so")
      :else (throw (RuntimeException. (str "Unsupported OS/arch: " os "/" arch))))
     :else (throw (RuntimeException. (str "Unsupported OS/arch: " os "/" arch))))))

(defn read-image-file
  "Recognize license plate from image file."
  [tsanpr imgfile output-format options]
  (printf "%s (outputFormat=\"%s\", options=\"%s\") => " imgfile output-format options)
  (let [result (.anpr_read_file tsanpr imgfile output-format options)]
    (println result)))

(defn read-encoded-image
  "Recognize license plate from encoded image buffer."
  [tsanpr imgfile output-format options]
  (printf "%s (outputFormat=\"%s\", options=\"%s\") => " imgfile output-format options)
  (try
    (let [encoded-img (Files/readAllBytes (Paths/get imgfile (make-array String 0)))
          result (.anpr_read_pixels tsanpr encoded-img (alength encoded-img) 0 0 "encoded" output-format options)]
      (println result))
    (catch IOException _
      (println "File open failed"))))

(defn read-pixel-buffer
  "Recognize license plate from pixel buffer."
  [tsanpr imgfile output-format options]
  (printf "%s (outputFormat=\"%s\", options=\"%s\") => " imgfile output-format options)
  (try
    (let [img (ImageIO/read (File. imgfile))]
      (if (nil? img)
        (println "Image load failed! (Unsupported format or file not found)")
        (let [width  (.getWidth img)
              height (.getHeight img)
              [stride pixel-format pixels]
              (condp = (.getType img)
                BufferedImage/TYPE_3BYTE_BGR
                [(* width 3) "BGR"
                 (let [raster (.getRaster img)
                       dbuf   (cast java.awt.image.DataBufferByte (.getDataBuffer raster))]
                   (.getData dbuf))]
                BufferedImage/TYPE_4BYTE_ABGR
                [(* width 4) "BGRA"
                 (let [raster (.getRaster img)
                       dbuf   (cast java.awt.image.DataBufferByte (.getDataBuffer raster))]
                   (.getData dbuf))]
                BufferedImage/TYPE_BYTE_GRAY
                [width "GRAY"
                 (let [raster (.getRaster img)
                       dbuf   (cast java.awt.image.DataBufferByte (.getDataBuffer raster))]
                   (.getData dbuf))]
                BufferedImage/TYPE_INT_RGB
                (let [raster (.getRaster img)
                      dbuf   (cast java.awt.image.DataBufferInt (.getDataBuffer raster))
                      int-pixels (.getData dbuf)
                      byte-buffer (byte-array (* (alength int-pixels) 4))]
                  [(* width 4) "BGRA" byte-buffer])
                BufferedImage/TYPE_INT_BGR
                (let [raster (.getRaster img)
                      dbuf   (cast java.awt.image.DataBufferInt (.getDataBuffer raster))
                      int-pixels (.getData dbuf)
                      byte-buffer (byte-array (* (alength int-pixels) 4))]
                  [(* width 4) "RGBA" byte-buffer])
                (do
                  (println (str "Unsupported BufferedImage type: " (.getType img)))
                  [0 nil nil]))]
          (when (and stride pixel-format pixels)
            (let [result (.anpr_read_pixels tsanpr pixels width height stride pixel-format output-format options)]
              (println result))))))
    (catch IOException e
      (println (str "Image load failed! (" (.getMessage e) ")")))))


(defn read-license-plates
  "Recognize license plates for a specific country code.

   NOTICE:
   anpr_initialize should be called only once after library load.
   Therefore, it is not possible to change the country code after anpr_initialize has been called.
   While using the free trial license, you can try all languages.
   When you purchase a commercial license, you can only use the selected language."
  [tsanpr country-code]
  (let [init-params (str "text;country=" country-code)
        error (.anpr_initialize tsanpr init-params)]
    (if (and error (not (empty? error)))
      (do
        (printf "anpr_initialize() failed (error=%s)\n" error)
        -1)
      (let [image-dir (str examples-base-dir "/img/" country-code "/")

            ;; TODO: Try each function as needed
            ;; (def anpr-fn read-image-file)
            ;; (def anpr-fn read-encoded-image)
            ;; (def anpr-fn read-pixel-buffer)
            anpr-fn read-image-file

            ;; TODO: Try each output format as needed
            ;; (def output-format "text")
            ;; (def output-format "json")
            ;; (def output-format "yaml")
            ;; (def output-format "xml")
            ;; (def output-format "csv")
            output-format "text"]

        ;; Single license plate recognition (default)
        (anpr-fn tsanpr (str image-dir "licensePlate.jpg") output-format "")

        ;; Recognize multiple license plates attached to vehicles
        (anpr-fn tsanpr (str image-dir "multiple.jpg") output-format "vm")

        ;; Recognize multiple license plates including motorcycles
        (anpr-fn tsanpr (str image-dir "multiple.jpg") output-format "vmb")

        ;; Recognize multiple license plates with surround detection
        (anpr-fn tsanpr (str image-dir "surround.jpg") output-format "vms")

        ;; Recognize multiple surrounding objects (vehicles)
        (anpr-fn tsanpr (str image-dir "surround.jpg") output-format "dms")

        ;; Recognize multiple surrounding objects and license plates
        (anpr-fn tsanpr (str image-dir "surround.jpg") output-format "dmsr")

        ;; Recognize multiple surrounding objects and license plates within RoI
        (anpr-fn tsanpr (str image-dir "surround.jpg") output-format "dmsri549,700,549,2427,1289,2427,1289,700")

        0))))

(defn -main
  "Main entry point for ANPR Clojure Example."
  [& args]
  ;; Set UTF-8 encoding for console output
  (System/setOut (PrintStream. System/out true StandardCharsets/UTF_8))
  (System/setErr (PrintStream. System/err true StandardCharsets/UTF_8))
  (println (str "Working directory: " (System/getProperty "user.dir")))

  (let [jni-path (get-jni-file-name)
        engine-path (get-engine-file-name)
        abs-jni-path (.getAbsolutePath (File. jni-path))
        abs-engine-path (.getAbsolutePath (File. engine-path))]
    (println (str "JNI wrapper absolute path: " abs-jni-path))
    (println (str "Engine absolute path: " abs-engine-path))
    (let [tsanpr (TSANPR. abs-jni-path abs-engine-path)]
      (try
        ;; TODO: Try each country code as needed
        (read-license-plates tsanpr "KR")
        ;; (read-license-plates tsanpr "JP")
        ;; (read-license-plates tsanpr "VN")
        (finally
          (.close tsanpr))))))
