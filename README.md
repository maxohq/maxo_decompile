# MaxoDecompile

[![Hex.pm](https://img.shields.io/hexpm/v/maxo_decompile.svg)](https://hex.pm/packages/maxo_decompile)
[![Docs](https://img.shields.io/badge/hexdocs-docs-8e7ce6.svg)](https://hexdocs.pm/maxo_decompile)
[![CI](https://github.com/maxohq/maxo_decompile/actions/workflows/ci.yml/badge.svg)](https://github.com/maxohq/maxo_decompile/actions/workflows/ci.yml)

`MaxoDecompile` is an Elixir / Erlang code decompiler.
This is a modifed and updated version from https://github.com/michalmuskala/decompile.
It has tests and the code is more maintainable.

## Installation

```bash
$ mix archive.install hex maxo_decompile
```

## Usage

```bash
# prints Elixir code to terminal ()
$ mix maxo.decompile MaxoDecompile.Core --to ex

# same as
$ mix maxo.decompile MaxoDecompile.Core --to ex --stdout=true

# prints Erlang code to terminal
$ mix maxo.decompile MaxoDecompile.Core --to erl

# prints ASM code to terminal
$ mix maxo.decompile MaxoDecompile.Core --to asm
```

The docs can be found at <https://hexdocs.pm/maxo_decompile>.

## Alternatives

- https://github.com/hrzndhrn/beam_file (much cleaner code and support for different Elixir versions, recommended!)
- https://github.com/michalmuskala/decompile


## Support

<p>
  <a href="https://quantor.consulting/?utm_source=github&utm_campaign=maxo_decompile">
    <img src="https://raw.githubusercontent.com/maxohq/sponsors/main/assets/quantor_consulting_logo.svg"
      alt="Sponsored by Quantor Consulting" width="210">
  </a>
</p>

## License

The lib is available as open source under the terms of the [MIT License](https://opensource.org/licenses/MIT).
