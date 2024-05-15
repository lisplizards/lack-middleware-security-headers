# lack-middleware-security-headers

## Usage

Wrap app:

```lisp
(funcall lack/middleware/charset:*lack-middleware-security-headers*
         *app*
         :content-security-policy "default-src 'self'; report-to http://reportcollector.example.com/collector.cgi")
```

Lack Builder:

```lisp
(lack:builder
 (:security-headers
  :x-frame-options "DENY"
  :x-xss-protection "0"
  :x-content-type-options "nosniff"
  :x-permitted-cross-domain-policies nil
  :referrer-policy "strict-origin-when-cross-origin"
  :strict-transport-security "max-age=300; includeSubdomains; preload"
  :content-security-policy "default-src https://www.my-site.example.com; report-to http://reportcollector.example.com/collector.cgi"
  :permissions-policy "accelerometer=(), ambient-light-sensor=(), autoplay=(), battery=(), camera=(), display-capture=(), document-domain=(), encrypted-media=(), execution-while-out-of-viewport=(), fullscreen=(), gamepad=(), geolocation=(self), gyroscope=(), hid=(), identity-credentials-get=(), idle-detection=(), local-fonts=(), magnetometer=(), microphone=(), midi=(), otp-credentials=(), payment=(), picture-in-picture=(), publickey-credentials-create=(), screen-wake-lock=(), serial=(), speaker-selection=(), storage-access=(), usb=(), web-share=(), window-management=(), xr-spatial-tracking=()"
  :additional-headers '(:x-foo "Foo"))
 *web*)
```

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-security-headers)
```

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
