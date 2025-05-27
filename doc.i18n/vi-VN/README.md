[English](../../README.md) | [한국어](../ko-KR/README.md) | [日本語](../ja-JP/README.md) | Tiếng Việt

# 😍TS-ANPR

**TS-ANPR** là công cụ nhận diện biển số xe dựa trên học sâu, hỗ trợ tiêu chuẩn biển số của Hàn Quốc, Nhật Bản và Việt Nam.

##### ✨ Ví dụ về ứng dụng (TS-IVR)

https://github.com/user-attachments/assets/71a2977a-4d1f-479b-a909-21c03fd9f013

##### [😍 Bản demo trực tiếp](http://tsnvr.ipdisk.co.kr/) <span style="font-size:.7em;font-weight:normal;color:grey">👈 Hãy trực tiếp kiểm tra hiệu suất nhận diện số tại đây.</span>

##### [🚀 Tải xuống động cơ mới nhất](https://github.com/bobhyun/TS-ANPR/releases/)

##### 🎨 Mẫu mã nguồn bằng các ngôn ngữ phổ biến

- [C](../../examples/C/) | [C#](../../examples/C#/) | [C++](../../examples/C++/) | [Clojure](../../examples/Clojure/) | [Dart](../../examples/Dart/) | [Delphi](../../examples/Delphi/) | [F#](../../examples/F#/) | [Go](../../examples/Go/) | [Haskell](../../examples/Haskell/) | [Java](../../examples/Java/) | [JavaScript](../../examples/JavaScript/) | [Julia](../../examples/Julia/) | [Kotlin](../../examples/Kotlin/) | [Lua](../../examples/Lua/) | [Perl](../../examples/Perl/) | [Python](../../examples/Python/) | [Ruby](../../examples/Ruby/) | [Rust](../../examples/Rust/) | [Scala](../../examples/Scala/) | [Swift](../../examples/Swift/) | [TypeScript](../../examples/TypeScript/) | [VB.NET](../../examples/VB.NET/)

##### 📖 Hướng dẫn phát triển ứng dụng

- [TS-ANPR](DevGuide.md)
- [TS-CAM](https://github.com/bobhyun/TS-CAM/blob/main/DevGuide.md)

##### [🎁 Hướng dẫn cài đặt](Usage.md)

##### [⚖️ Giấy phép](LICENSE.md)

_Nếu bạn có bất kỳ câu hỏi hoặc yêu cầu nào, vui lòng gửi tại [Issues](https://github.com/bobhyun/TS-ANPR/issues).
Chúng tôi luôn sẵn sàng hỗ trợ và rất mong nhận được phản hồi từ bạn!_

- Liên hệ: 📧 skju3922@naver.com

---

## Mục lục

- [Thông tin phiên bản mới nhất](#thông-tin-phiên-bản-mới-nhất)
- [Các loại mô hình học sâu và ứng dụng của chúng](#các-loại-mô-hình-học-sâu-và-ứng-dụng-của-chúng)
- [Bảng so sánh tốc độ nhận diện theo từng mô hình học sâu](#bảng-so-sánh-tốc-độ-nhận-diện-theo-từng-mô-hình-học-sâu)
- [Đặc điểm nổi bật](#đặc-điểm-nổi-bật)
- [Nhiều tùy chọn nhận diện](#nhiều-tùy-chọn-nhận-diện)

<br/>

---

## Thông tin phiên bản mới nhất

#### v3.0.0 phát hành (2025.5.27)🎉

1. Thêm chức năng nhận diện biển số xe Nhật Bản và Việt Nam

   - Hỗ trợ 140 khu vực tại Nhật Bản, bao gồm biển số ngoại giao, Lực lượng Phòng vệ và biển số kiểu cũ.

   - Hỗ trợ biển số xe ô tô và xe máy tại Việt Nam.

   - Hỗ trợ quốc gia tùy theo loại giấy phép:

     - Giấy phép dùng thử miễn phí: Chỉ định quốc gia bằng thiết lập country trong hàm khởi tạo của ứng dụng. ([Chi tiết](DevGuide.md#11-anpr_initialize))
     - Giấy phép thương mại: Thiết lập country trong hàm khởi tạo sẽ bị bỏ qua và quốc gia được xác định theo giấy phép đã mua.

2. Thêm tính năng thiết lập vùng quan tâm (RoI) / vùng không quan tâm (RoU)
   - Có thể đặt vùng nhận diện biển số trong hình ảnh đầu vào. ([Chi tiết](DevGuide.md#23-thiết-lập-vùng-quan-tâm-roi--vùng-không-quan-tâm-rou))
     ![](../../img/options/roi.png)
3. Thêm tính năng cài đặt kích thước tối thiểu biển số

   - Khi cài đặt kích thước tối thiểu, các vùng biển số nhỏ hơn sẽ bị bỏ qua. ([Chi tiết](DevGuide.md#24-thiết-lập-kích-thước-tối-thiểu-của-biển-số))

4. Cải thiện tỷ lệ nhận diện
   - Độ chính xác nhận diện biển số được nâng cao nhờ cải tiến thuật toán.
5. Tách module `tscam`
   - Module `tscam` được tách khỏi bản phân phối `TS-ANPR` và phát hành tại [TS-CAM Releases](https://github.com/bobhyun/TS-CAM/releases)
   - Hỗ trợ camera HTTPS sử dụng chứng chỉ tự ký

## Các loại mô hình học sâu và ứng dụng của chúng

Giấy phép được áp dụng cho tất cả các mô hình học sâu, vì vậy bạn chỉ cần chọn mô hình phù hợp nhất với mục đích sử dụng của mình.

| Mô hình |                 Tốc độ nhận diện                  | Ví dụ áp dụng                                                                                                                                                                                                                                                                              |
| :-----: | :-----------------------------------------------: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|  **S**  |       Nhanh<br/>(dùng cho khoảng cách ngắn)       | Quản lý ra vào bãi đỗ xe<br/><img src="../../img/models/small1.png" />                                                                                                                                                                                                                     |
|  **M**  | Bình thường<br/>(dùng cho khoảng cách trung bình) | Quản lý chỗ đỗ xe trống/đầy / Tìm vị trí đỗ xe<br/><img src="../../img/models/medium1.png" /><br/>Camera ống kính mắt cá (nhận diện 360° toàn cảnh)<br/><img src="../../img/models/medium2.png" /><br/>Xe bị lật (nhận diện 360° toàn cảnh)<br/><img src="../../img/models/medium3.png" /> |
|  **L**  |        Chậm<br/>(dùng cho khoảng cách xa)         | Bãi đỗ xe ngoài trời quy mô lớn / Đếm số lượng xe<br/><img src="../../img/models/large1.png" /><br/>Nhận diện biển số xe nhiều làn<br/><img src="../../img/models/large2.png" /><br/>Thống kê lưu lượng giao thông<br/><img src="../../img/models/large3.png" />                           |

## Bảng so sánh tốc độ nhận diện theo từng mô hình học sâu

| CPU                               | Cores | Threads | Clock<sup>(1)</sup> | OS                              | S<sup>(2)</sup> | M<sup>(2)</sup> | L<sup>(2)</sup> |
| --------------------------------- | ----: | ------: | ------------------: | :------------------------------ | --------------: | --------------: | --------------: |
| Intel i7-12700                    |    12 |      20 |                 2.1 | Windows 64-bit<br/>Linux 64-bit |           0.021 |           0.036 |           0.054 |
| Intel i5-6500                     |     4 |       4 |                 3.2 | Windows 64-bit<br/>Linux 64-bit |           0.031 |           0.078 |           0.140 |
| (Như trên)                        |       |         |                     | Windows 32-bit                  |           0.078 |           0.172 |           0.296 |
| Intel i3-8100                     |     4 |       4 |                 3.6 | Windows 64-bit<br/>Linux 64-bit |           0.042 |           0.087 |           0.156 |
| (Như trên)                        |       |         |                     | Windows 32-bit                  |           0.089 |           0.204 |           0.656 |
| Intel Celeron J4005               |     2 |       2 |                 2.0 | Windows 64-bit<br/>Linux 64-bit |           0.396 |           0.886 |           1.563 |
| (Như trên)                        |       |         |                     | Windows 64-bit                  |           0.629 |           1.355 |           2.368 |
| Intel Celeron 1037U<sup>(3)</sup> |     2 |       2 |                 1.8 | Windows 32-bit                  |           0.484 |           1.061 |           1.856 |
| Rockchip RK3588S<sup>(4)</sup>    |     8 |       8 |                 1.5 | Linux 64-bit                    |           0.227 |           0.462 |           0.842 |
| Broadcom BCM2711<sup>(5)</sup>    |     4 |       4 |                 1.8 | Linux 64-bit                    |           0.465 |           1.024 |           1.817 |

- Đo bằng hình ảnh chỉ có một chiếc xe ô tô
- <sup>(1)</sup> Đơn vị: GHz
- <sup>(2)</sup> Đơn vị: Giây
- <sup>(3)</sup> CPU chỉ hỗ trợ 32-bit [(Xem thông số kỹ thuật của nhà sản xuất)](https://www.intel.co.kr/content/www/kr/ko/products/sku/71995/intel-celeron-processor-1037u-2m-cache-1-80-ghz/specifications.html)
- <sup>(4)</sup> NanoPi R6S [(Xem thông số kỹ thuật của nhà sản xuất)](https://www.friendlyelec.com/index.php?route=product/product&product_id=289)
- <sup>(5)</sup> Raspberry Pi4 [(Xem thông số kỹ thuật của nhà sản xuất)](https://www.raspberrypi.com/products/raspberry-pi-4-model-b/)

## Đặc điểm nổi bật

#### 1. Hiệu suất nhận diện biển số xe

Có khả năng thích nghi vượt trội với các yếu tố môi trường dưới đây.

- Phim phản quang (biển số Hàn Quốc)
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/film1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film10.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film11.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film12.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film13.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film14.jpg" />
  <div>
- Nhiễu ban đêm
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise9.jpg" />
  <div>
- Góc chụp
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle10.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle11.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle12.jpg" />
  </div>
- Thời tiết / Chiếu sáng
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/light1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light10.jpg" />
  </div>
- Ô nhiễm / Hư hại
  <div>    
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty10.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty8.jpg" />
  </div>
- Ảnh camera mắt cá 360 độ.
  - _Nhận diện nhiều biển số xe trực tiếp từ ảnh gốc mà không cần dewarping._
  <div>
    <img style="margin-right:-5px" src="../../img/ex/fisheye1.jpg" />
  </div>

#### 2. Hỗ trợ nhiều loại định dạng biển số xe

Hỗ trợ các loại định dạng biển số xe đa dạng như dưới đây.

- Biển số Hàn Quốc

  - Biển số xe ben, xe thiết bị thi công
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq6.jpg" />
    </div>
  - Biển số đặc biệt (tạm thời, ngoại giao, quân sự)
    <div>
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/mil1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/mil2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/mil3.jpg" />
    </div>
  - Biển số xe điện thân thiện với môi trường
    - Kết quả nhận diện biển số sẽ phân biệt xe điện thân thiện với môi trường.
    - Tuy nhiên, nếu định dạng biển số không phân biệt được xe điện với xe động cơ đốt trong như biển số xe thương mại, thì không thể xác định được.
    <div>
      <img style="margin-right:-5px" width="120" src="../../img/ex/ev2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/ev1.jpg" />
    </div>
  - Biển số kiểu cũ thập niên 80, 90
    - Hỗ trợ các ký tự ‘처’, ‘퍼’, ‘차’, ‘파’, ‘추’ đến ‘후’, và ‘그’ đến ‘흐’ được sử dụng trước khi sửa đổi quy cách biển số năm 1996.
    - Hỗ trợ định dạng biển số kiểu cũ của quân đội Mỹ tại Hàn Quốc.
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/801.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/802.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/803.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/804.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/805.jpg" />
    </div>

- Biển số xe Nhật Bản
  - Biển số đặc biệt (ngoại giao, Lực lượng Phòng vệ)
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep5.jpg" />
    </div>
  - Biển số kiểu cũ từ những năm 1960
    - Hỗ trợ định dạng biển số kiểu cũ chỉ hiển thị một ký tự cho tên khu vực (ví dụ: 東, 京, 名, v.v.).
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-601.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-602.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-603.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-604.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-605.jpg" />
    </div>

#### 3. Hỗ trợ hệ điều hành / kiến trúc CPU chính

- Windows
  - Hỗ trợ kiến trúc Intel 64-bit (windows-x86_64) và 32-bit (windows-x86)
  - Tương thích với Windows 7 trở lên
- Linux
  - Hỗ trợ kiến trúc Intel 64-bit (linux-x86_64)
  - Hỗ trợ kiến trúc ARM 64-bit (linux-aarch64)
  - Tương thích với mọi bản phân phối sử dụng glibc 2.27 trở lên

#### 4. Hỗ trợ nhiều môi trường phát triển

- Giao diện thư viện đa năng, không phụ thuộc vào ngôn ngữ lập trình cụ thể
  - [Cung cấp ví dụ cho từng ngôn ngữ lập trình](../../examples/)
- [Định dạng tệp ảnh đầu vào](DevGuide.md#12-anpr_read_file)
  - `bmp`, `jpg`, `png`, `pnm`, `pbm`, `pgm`, `ppm`, `jfif`, `webp`
- [Định dạng pixel bộ đệm bộ nhớ ảnh đầu vào](DevGuide.md#13-anpr_read_pixels)
  - `GRAY`, `BGRA`, `RGBA`, `RGB`, `BGR`, `BGR555`, `BGR565`, `HSV`, `YCrCb`, `I420`, `YV12`, `IYUV`, `NV12`, `NV21`
- [Định dạng xuất kết quả nhận diện](DevGuide.md#3-output-formats)
  - `text`, `csv`, `json`, `yaml`, `xml`

#### 5. Cung cấp nhiều loại giấy phép

- Giấy phép dùng thử miễn phí
  - Cung cấp 30 ngày dùng thử miễn phí cho mỗi hệ thống sau khi cài đặt để phát triển và trình diễn
- Giấy phép thương mại
  - Theo phương tiện: Chọn giữa USB dongle hoặc giấy phép phần mềm
  - Theo tính năng và hiệu suất: Có thể chọn `Basic`, `Object Detection`, `Pro`, hoặc `Server` tùy theo yêu cầu phần mềm ứng dụng (Tham khảo: [Động cơ TS-ANPR](LICENSE.md#2-động-cơ-ts-anpr))

## Nhiều tùy chọn nhận diện

#### 1. Kiểm tra biển số gắn trên xe

Phân biệt biển số có được gắn trên xe trong hình ảnh có thân xe xuất hiện.
Khi sử dụng tùy chọn **Gắn trên xe (v)**, chỉ nhận diện biển số được gắn trên xe.<br/>
<img width="500" src="../../img/mounted1.jpg" />

Biển số không có xe đi kèm hoặc biển số xe máy như trong hình dưới đây sẽ bị bỏ qua.<br/>

<img width="500" src="../../img/mounted2.jpg">
<div style="font-size:0.8em">[Nguồn hình ảnh: 연합뉴스]</div>
</img>

<br/>

<img width="500" src="../../img/mounted2-1.jpg">
<div style="font-size:0.8em">[Nguồn hình ảnh: 바이커즈랩]</div>
</img>

<br/>

Nếu chỉ chụp cận cảnh biển số, hệ thống có thể không nhận diện được xe. Trong trường hợp này, nếu không sử dụng tùy chọn **Gắn trên xe (v)**, vẫn có thể nhận diện được biển số xe.<br/>
<img width="500" src="../../img/mounted3.jpg" />

#### 2. Nhận diện nhiều đối tượng

Khi sử dụng tùy chọn **Nhận diện nhiều đối tượng (m)**, tất cả các xe trong ảnh đều được nhận diện.<br/>
<img width="800" src="../../img/multiple1.jpg" />

Nếu không sử dụng tùy chọn **Nhận diện nhiều đối tượng (m)**, chỉ biển số xe có độ tin cậy cao nhất (dễ nhìn thấy nhất) trong số nhiều xe sẽ được nhận diện.<br/>
<img width="800" src="../../img/multiple2.jpg" />

#### 3. Nhận diện 360° toàn cảnh

Khi sử dụng tùy chọn **Nhận diện 360° toàn cảnh (s)**, biển số xe vẫn có thể được nhận diện ngay cả khi xe trong ảnh bị nghiêng hoặc lật ở nhiều hướng khác nhau, như xe bị lật hoặc xe được chụp bằng camera mắt cá.<br/>

<img width="800" src="../../img/surround1.jpg">
<div style="font-size:0.8em">[Nguồn hình ảnh: KBS]</div>
</img>

<br/>

<img width="800" src="../../img/surround2.jpg" />

#### 4. Nhận diện đối tượng

Khi sử dụng tùy chọn **Nhận diện đối tượng (d)**, các đối tượng trong ảnh sẽ được nhận diện.
Bằng cách so sánh vùng xe được phát hiện với vùng đỗ xe do ứng dụng thiết lập, có thể xác định chỗ đó đã có xe hay còn trống.<br/>

<img width="800" src="../../img/options/dms.png" />

#### 5. Nhận diện biển số xe của đối tượng

Khi sử dụng đồng thời tùy chọn **Nhận diện đối tượng (d)** và **Nhận diện biển số xe (r)**, biển số của các xe được nhận diện cũng sẽ được nhận diện.<br/>

<img width="800" src="../../img/options/dmsr.png" />

#### 5. Thiết lập vùng quan tâm và kích thước biển số tối thiểu

Bằng cách kết hợp thiết lập **vùng quan tâm (i)**, **vùng loại trừ (x)** và **kích thước biển số tối thiểu (a)**, có thể ngăn nhận diện biển số xe ngoài vùng quan tâm.<br/>

<img width="800" src="../../img/options/roi.png" />

---

- Để kiểm tra hiệu suất cơ bản trước khi phát triển ứng dụng, bạn có thể sử dụng [bản demo trực tiếp](http://tsnvr.ipdisk.co.kr/).
- Trong giai đoạn phát triển ứng dụng, vui lòng tham khảo [Hướng dẫn phát triển ứng dụng](DevGuide.md) và các ví dụ theo từng ngôn ngữ lập trình đi kèm.
