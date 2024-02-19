This creates a .json trace file, compatible with Perfetto: https://www.ui.perfetto.dev/

    $ mpl -trace true -trace-runtime true foo.sml
    $ mltrace record ./foo
    $ mltrace exportj
