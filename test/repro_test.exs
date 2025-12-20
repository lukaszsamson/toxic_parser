defmodule ToxicParser.ReproTest do
  use ExUnit.Case, async: false

  test "repro 1" do
    code = "&foo/1..0..case Foo.eggs() do\n  foo -> qux\n  _ -> ?a\nend//0"
    assert_conforms(code)
  end

  test "repro 2" do
    code = "@foo Foo.\"a\"(with ^delta <- beta do\n  ?y\nelse\n  _ -> 0.0\nend)"
    assert_conforms(code)
  end

  test "repro 2a" do
    code = "@foo Foo.a(try do\n :ok\nend)"
    assert_conforms(code)

    code = "@foo Foo.\"a\"(try do\n :ok\nend)"
    assert_conforms(code)
  end

  test "repro 3" do
    code = "Foo.foo() |> {foo..:ok..['two': :ok]//0, [['baz': spam]]}"
    assert_conforms(code)
  end

  test "repro 4" do
    code = "{foo, Foo.foo()}..\"\" <> \"foo\#{%{\"K\" => Mod}}bar\""
    assert_conforms(code)
  end

  test "repro 5" do
    code = "foo() + 'W'..foo..0//0"
    assert_conforms(code)
  end

  test "repro 6" do
    code = "foo..foo..fn -> :ok end//0"
    assert_conforms(code)
  end

  test "repro 7" do
    code = "0 + 'A'..\"foo\#{foo}bar\" <> \"\"..%{label: &3}//0"
    assert_conforms(code)
  end

  test "repro 8" do
    code = "!case 1 do\n  18.0 -> 49.0\nend * foo"
    assert_conforms(code)

    code = "not case 1 do\n  18.0 -> 49.0\nend * foo"
    assert_conforms(code)

    code = "-case 1 do\n  18.0 -> 49.0\nend * foo"
    assert_conforms(code)

    code = "+case 1 do\n  18.0 -> 49.0\nend - foo"
    assert_conforms(code)
  end

  test "repro 9a" do
    code = "not try do\n :ok\nend <= 1"
    assert_conforms(code)
  end

  test "repro 10" do
    code =
      "[@spec foo() :: term(), with gamma <- beta do\n  Bar\nelse\n  _ -> 0\nend |> {-7, qux} + ~s\"\"\"\nfoo \#{0.0} bar\n\"\"\"]"

    assert_conforms(code)
  end

  test "repro 10a" do
    code =
      "[@foobar foo() :: term(), with gamma <- beta do\n  Bar\nelse\n  _ -> 0\nend |> {-7, qux} + ~s\"\"\"\nfoo \#{0.0} bar\n\"\"\"]"

    assert_conforms(code)
  end

  test "repro 11" do
    code = "['foo\#{%Foo{\"end\": 47.0}}bar' or foo |> baz..:error..gamma//2]"
    assert_conforms(code)
  end

  test "repro 12" do
    code = "with [label: [\"baz\": ^bar]] <- @type baz :: any()..qux..22.0//1, do: \"\""
    assert_conforms(code)
  end

  test "repro 13" do
    code =
      "%{\"z9H\" => not 'foo\#{beta}bar'} |> \"foo\#{spam |> Context}bar\"..%Qux{\"one\": :one}..\"\" <> \"\"//1"

    assert_conforms(code)
  end

  test "repro 14" do
    code =
      "{{[qux, :error, beta] - ?l..gamma, %Remote{'alice': \"foo\#{qux}bar\" <> \"\"}}, {%{\"N\" => 41.0 >>> ?m}, [Default |> ?k, ?i ** qux]}, \"foo\#{[\"two\": spam]}bar\"}"

    assert_conforms(code)
  end

  test "repro 15" do
    code = "{11.0, [eggs, ?g, Qux] |> {:ok, 0}}..\"\" <> \"foo\#{['baz': :one]}bar\""
    assert_conforms(code)
  end

  test "repro 16" do
    code = "Foo.foo(\"foo\#{\"foo\#{[\"alice\": 0]}bar\"}bar\")"
    assert_conforms(code)
  end

  test "repro 17" do
    code =
      "quote do: case \"foo\#{gamma}bar\" <> \"foo\#{14.0}bar\"..\"\" <> \"foo\#{eggs}bar\" do\n  ?t -> [opts: gamma]\n  _ -> %Baz{\"ok\": Bar}\nend"

    assert_conforms(code)
  end

  test "repro 18" do
    code = "'foo\#{@beta with Config <- :bar, do: :foo + [Baz]}bar'"
    assert_conforms(code)
  end

  test "repro 19" do
    code = "@baz with Foo <- Context, do: ?x + &(@eggs %{label: alpha} + 1)"
    assert_conforms(code)
  end

  test "repro 20" do
    code =
      "case <<[\"two\": case beta do\n  38.0 -> qux\n  _ -> ?d\nend]>> do\n  [\"three\": ?m] -> Bar.eggs()\n  _ -> 0.0\nend"

    assert_conforms(code)
  end

  test "repro 21" do
    code = "-quote do\n baz\nend ||| a"
    assert_conforms(code)

    code = "+quote do\n baz\nend === a"
    assert_conforms(code)

    code = "!quote do\n baz\nend != a"
    assert_conforms(code)

    code = "not quote do\n baz\nend == a"
    assert_conforms(code)

    code = "not quote do\n baz\nend or a"
    assert_conforms(code)

    code = "not quote do\n baz\nend = a"
    assert_conforms(code)

    code = "not quote do\n baz\nend && a"
    assert_conforms(code)

    code = "not quote do\n baz\nend &&& a"
    assert_conforms(code)

    code = "not quote do\n baz\nend and a"
    assert_conforms(code)

    code = "not quote do\n baz\nend || a"
    assert_conforms(code)
  end

  test "repro 22" do
    code =
      "case State.\"two\"(quote do\n  %Mod{'alice': :baz}\nend) do\n  ['three': :\"ok\"] -> %{\"end\": ?z} |> delta\n  _ -> %Foo{'error': %Foo{label: baz}}\nend"

    assert_conforms(code)
  end

  test "repro 23" do
    code = "@delta case Config.alpha(qux, beta) do\n  Context -> ?u\n  _ -> 3\nend..Foo"
    assert_conforms(code)
  end

  test "repro 24a" do
    code = "@foo try do\n  1\nrescue\n_ -> 0\nend..1"
    assert_conforms(code)
  end

  test "repro 24b" do
    code = "@foo try do\n  1\nend..1//2"
    assert_conforms(code)
  end

  test "repro 25" do
    code = "not [?r]..29.0 ++ gamma |> [%Foo{label: eggs}, [\"end\": Bar]]"
    assert_conforms(code)
  end

  test "repro 25a1" do
    code = "not a..2//3 - gamma"
    assert_conforms(code)

    code = "not a..2 ++ gamma"
    assert_conforms(code)
  end

  test "repro 25a" do
    code = "not b |> a"
    assert_conforms(code)

    code = "!b |> a"
    assert_conforms(code)

    code = "+b |> a"
    assert_conforms(code)

    code = "-b |> a"
    assert_conforms(code)

    code = "^b |> a"
    assert_conforms(code)
  end

  test "repro 26a" do
    code = "1..(&not/2)//0"
    assert_conforms(code)
  end

  test "repro 27" do
    code =
      "fn ['end': baz], ^qux -> &({-5, foo} + 1) in Remote.\"three\"(\"foo\#{foo}bar\", not spam) end"

    assert_conforms(code)
  end

  test "repro 28" do
    code = "<<:'ok' + %{\"A\" => delta}>> |> not 0 |> [\"bar\": Foo] + ['bob': foo] + :ok"
    assert_conforms(code)
  end

  test "repro 29" do
    code = "&('''\nfoo \#{%{'ok': alpha}} bar\n''' + 1)..0.0"
    assert_conforms(code)
  end

  test "repro 30" do
    code = "\"foo\#{[{:alice, :alice}]}bar\" + not %{'ok': -1} |> quote do: :ok"
    assert_conforms(code)
  end

  test "repro 31" do
    code =
      "case \"\" <> \"foo\#{9.0}bar\"..['do': \"\"\"\nfoo \#{spam} bar\n\"\"\"] do\n  <<:\"bar\">> -> ~S/\#{spam}/i\n  _ -> <<?a..foo>>\nend"

    assert_conforms(code)
  end

  test "repro 31a" do
    code = "case \"\" <> \"foobar\"..['dos': \"\"\"\nfoo  bar\n\"\"\"] do\n  :ok\nend"
    assert_conforms(code)
  end

  test "repro 32" do
    code = "&(['one': :ok] + 1) |> Foo.foo()"
    assert_conforms(code)
  end

  test "repro 33" do
    code = "{:ok, <<+quote do\n  spam\nend, with ^eggs <- :bob do\n  ?p\nelse\n  _ -> 0\nend>>}"
    assert_conforms(code)
  end

  test "repro 34" do
    code = "not a <> b"
    assert_conforms(code)

    code = "not \"\" <> \"\""
    assert_conforms(code)
  end

  test "repro 35" do
    code = "&(&(%Qux{label: ['alice': foo]} + 1) + 1)"
    assert_conforms(code)
  end

  test "repro 36" do
    code = "Foo.foo('M' |> %Baz{\"ok\": \"\"\"\nfoo \#{alpha} bar\n\"\"\"})"
    assert_conforms(code)
  end

  test "repro 36a" do
    code = "foo('M' |> %{\"ok\": :err})"
    assert_conforms(code)
  end

  test "repro 37" do
    code = "~s\"\"\"\nfoo \#{&(&(0 + 1) + 1)} bar\n\"\"\""
    assert_conforms(code)
  end

  test "repro 38" do
    code = "not ~s'\#{?a}' |> %{\"k\" => 5} < @qux 0"
    assert_conforms(code)
  end

  test "repro 39" do
    code = "not foo |> foo |> foo < '''\nfoo \#{0.0 |> foo} bar\n'''"
    assert_conforms(code)
  end

  test "repro 40" do
    code = "0 |> not \"\"\"\nfoo \#{foo} bar\n\"\"\" + \"\"\"\nfoo \#{{foo, foo}} bar\n\"\"\""
    assert_conforms(code)
  end

  test "repro 41" do
    code =
      "case not \"foo\#{:do}bar\" <> \"foo\#{State}bar\" do\n  {alpha, ^alpha} -> \"\"\"\nfoo \#{baz} bar\n\"\"\"\n  _ -> foo()\nend |> \"\"\"\nfoo \#{foo + Foo |> +delta} bar\n\"\"\""

    assert_conforms(code)
  end

  test "repro 42" do
    code = "'foo\#{&(0 |> Foo + 1) |> %{label: [Config, ?o, Qux]}}bar'"
    assert_conforms(code)
  end

  test "repro 43" do
    code =
      "&('foo\#{Context |> 22.0}bar' + 1) in fn -> with ^spam <- bar do\n  Bar\nelse\n  _ -> gamma\nend end |> quote do\n  20.0\nend"

    assert_conforms(code)
  end

  test "repro 44" do
    code =
      "+case {delta, -4} do\n  foo -> gamma\n  _ -> :error\nend |> :alice ** foo >>> %Default{\"bar\": alpha}"

    assert_conforms(code)
  end

  test "repro 45" do
    code = "Context.foo(%{\"v\" => &(?h + 1) !== \"Tsyh\"})"
    assert_conforms(code)
  end

  test "repro 46" do
    code = "+with ?p <- :three do\n  beta\nelse\n  _ -> foo\nend..Schema//2"
    assert_conforms(code)
  end

  test "repro 47" do
    code = "fn gamma -> Context end &&& &(gamma + 1) |> %Baz{metadata: :'alice'} |> Config"
    assert_conforms(code)
  end

  test "repro 48" do
    code =
      "not gamma |> :foo > [\"alice\": alpha] |> case &1 do\n  ^delta -> ?v\n  _ -> delta\nend"

    assert_conforms(code)
  end

  test "repro 49" do
    code = "&(not \"foo\#{-3}bar\" <> \"foo\#{eggs}bar\" + 1)"
    assert_conforms(code)
  end

  test "repro 50" do
    code = "&(%{\"three\": beta |> 0} + 1) ++ State.\"foo\"(bar)"
    assert_conforms(code)
  end

  test "repro 51" do
    code = """
    -not delta 27.51 + if false do
    148
    end; !:alice
    """

    assert_conforms(code)
  end

  test "repro 51a" do
    code = """
    not delta 27.51 + if false do
    148
    end; :alice
    """

    assert_conforms(code)

    code = """
    @ delta 27.51 + if false do
    148
    end; :alice
    """

    assert_conforms(code)

    code = """
    ^ delta 27.51 + if false do
    148
    end
    :alice
    """

    assert_conforms(code)

    code = """
    & delta 27.51 + if false do
    148
    end; :alice
    """

    assert_conforms(code)

    code = """
    -delta 27.51 + if false do
    148
    end; :alice
    """

    assert_conforms(code)

    code = """
    1..2//delta 27.51 + if false do
    148
    end; :alice
    """

    assert_conforms(code)
  end

  test "repro 52" do
    code = """
    not ... - unless true do
    false
    end
    State !== 180 === case Config do
    eggs -> nil
    :error -> alpha
    :foo -> 46.34
    end
    if true do
    delta
    delta
    end
    0
    """

    assert_conforms(code)
  end

  test "repro 53" do
    code = """
    not foo :baz
    ..
    if false do
    ?v
    0xD9
    else
    beta
    end; & bar true
    (Mod)
    ; if true do
    90.32
    Remote
    else
    Foo
    end
    """

    assert_conforms(code)
  end

  test "repro 54" do
    code = """
    Config && ..
    ; ..
    case Schema do
    bar -> spam
    gamma -> eggs
    :foo -> Mod
    end
    -248; 74.0
    ...Qux
    &bar >= unless delta do
    true
    baz
    else
    -763
    end
    ..
    -343 - 0b11.(beta, ?n) <- ...&1
    """

    assert_conforms(code)
  end

  test "repro 54a" do
    code = """
    &bar >= foo()
    <>
    1 <- ...1
    """

    assert_conforms(code)

    code = """
    &bar >= unless delta do
    true
    end
    +
    1 <- ...1
    """

    assert_conforms(code)

    code = """
    &bar >= unless delta do
    true
    end
    <>
    1 <- 2
    """

    assert_conforms(code)
  end

  test "repro 55" do
    code = """
    ...@@spam[Context]
    """

    assert_conforms(code)
  end

  test "repro 56" do
    code = """

    [@
    eggs, @
    eggs[:error], @
    @bar[89.35]]
    &{alpha, foo, false}; (; :one -> Default)
    @foo[foo] <~ delta beta: 0o50 |> foo.delta gamma
    <~> -[0o36, 729, Foo]
    """

    assert_conforms(code)
  end

  test "repro 57" do
    # . and @ operator precedence bug
    code = """
    @Foo.Bar
    """

    assert_conforms(code)
  end

  test "repro 58" do
    code = """
    cond .. bar // spam <-
    ... +++ ( delta -> Default.Schema.State; qux -> 254)
    baz Baz.Config
    ...bar.spam; @ bar alpha: delta; [baz[60], (; :error -> :ok; foo -> ?r) <= .., @@:ok[true]]; ^
    spam
    @fn bar, beta, alpha, qux -> :bob end <= &3
    ; @ -799 <|> false.when :bar
    fn -> State end == false when State.Context.Default.bar; eggs \\ @ ( alpha -> Baz; delta -> Bar)
    ; ... <> fn spam, qux when gamma and true -> Remote.Foo end\
    """

    assert_conforms(code)
  end

  test "repro 59" do
    code = """
    ... &4 .. for // ...
    delta <|> bar -604
    not spam .. (;) //
    ...
    ~~~
    ( bar -> Context.Config.Schema; 4 -> spam) ~>> ()
    spam 0b101 != @ Bar.Default.Remote; fn eggs, beta, baz -> gamma | .. end
    ~~~&8
    {{478}, (;) ..
    (; foo -> nil; 4 -> bar) // baz}
    {} <<~
    spam.foo beta: Mod.Context --- (;)
    ... ... Default.Bar.State
    842 .. @fn foo -> :bob end // !
    State
    - (:two)
    qux fn -> true end <~> spam ?a\
    """

    assert_conforms(code)
  end

  test "repro 60" do
    code = """
    ( delta -> spam; baz -> false)
    - baz <<~ Mod.Remote.State.eggs 195
    @@foo[?l] when qux ^^^ 71.28
    ; ...qux(beta, bar, beta) ~>
    ( :bob -> Config)
    ; gamma(baz) ~> ...spam
    ?\\\\.(@
    gamma, true != beta, delta[Mod])
    !
    @Context; @ ... ||| (; foo -> 0x0)\
    """

    assert_conforms(code)
  end

  test "repro 61" do
    code = """
    .. || @ Bar.Schema + 198
    ( alpha -> Default; qux -> qux)
    -16.(fn eggs -> delta - beta end)
    ![ ... .. (;) // eggs, beta < [beta, eggs], (; :bar -> 2.78) \\\\ foo]\
    """

    assert_conforms(code)
  end

  test "repro 61a" do
    code = """
    .. || @ Bar.Schema + 198
    ( alpha -> Default; qux -> qux)
    -16.(fn eggs -> delta - beta end)
    ![ ... .. (;) // eggs, beta < [beta, eggs], (; :bar -> 2.78) \\ foo]\
    """

    assert_conforms(code)
  end

  test "repro 62" do
    code = """
    nil .. nil
    bar
    (; foo -> 203; 8 -> State.Context); &baz
    ; nil !== nil
    @
    Config.State.gamma
    """

    assert_conforms(code)
  end

  test "repro 63" do
    code = """
    @@delta[?a]\
    """

    assert_conforms(code)
  end

  test "repro 64" do
    code = """
    @@foo -- 999\
    """

    assert_conforms(code)
  end

  test "repro 65" do
    code = """
    @@bar.(0x0)\
    """

    assert_conforms(code)
  end

  test "repro 66" do
    code = """
    ~~~ spam()()
    -spam()()
    not spam()()
    !spam()()
    &spam()()
    @spam()()
    1..2//spam()()
    """

    assert_conforms(code)
  end

  test "repro 67" do
    code = """
    & bar !== Baz.Foo.Context.(0xC7) do
    bar
    end
    """

    assert_conforms(code)
  end

  test "repro 68" do
    code = """

    ... alpha()(delta) do; end
    @ Foo.eggs(false, gamma) do
    baz
    delta
    else
    :alice
    859
    end

    """

    assert_conforms(code)
  end

  test "repro 69" do
    code = """
    @ foo spam: gamma do Foo.Baz
    beta
    end <~> :one.- gamma ~> bar bar: :bar, gamma: Foo
    """

    assert_conforms(code)
  end

  test "repro 69a" do
    code = """
    foo gamma do Foo.Baz
    beta
    end
    """

    assert_conforms(code)
  end

  test "repro 70" do
    code = """
    ... 481.(true, State) do bar
    :bob
    end
    """

    assert_conforms(code)
  end

  test "repro 71" do
    code = """
    @ & if true do
    :ok
    end when if false do
    qux
    end
    """

    assert_conforms(code)
  end

  test "repro 72" do
    code = """
    ... gamma.if true do
      baz
    end \\\\ receive true do; gamma
    Remote.Config.Qux
    end
    """

    assert_conforms(code)
  end

  test "repro 73" do
    code = """
    ... & if delta do
    :ok
    end <- foo
    """

    assert_conforms(code)

    code = """
    & & if delta do
    :ok
    end <- foo
    """

    assert_conforms(code)

    code = """
    + & if delta do
    :ok
    end <- foo
    """

    assert_conforms(code)

    code = """
    @ & if delta do
    :ok
    end <- foo
    """

    assert_conforms(code)
  end

  test "repro 74" do
    code = """
    & delta()() do
      baz
    end
    """

    assert_conforms(code)
  end

  test "repro 75" do
    code = """
    ; Mod.baz spam: 0o11 do
    :two
    :foo
    rescue
    beta -> Qux
    end
    nil.~>> eggs: false, qux: spam do Remote
    Context
    rescue
    alpha -> alpha
    after
    Schema
    end

    """

    assert_conforms(code)
  end

  test "repro 75a" do
    code = """
    & baz eggs do Context.Qux.Baz
    beta
    rescue
    :error -> spam
    1 -> Baz.Foo.Bar
    after
    gamma
    State
    end
    eggs &&
    @ ( beta -> bar) <|> qux spam: Mod.Remote.Bar, eggs: eggs

    """

    assert_conforms(code)
  end

  test "repro 76" do
    code = """
    gamma.if baz do false
    rescue
    spam -> -679
    :ok -> ?q
    after
    baz
    0b111
    end
    not gamma .. baz // (;) <|> qux +foo
    """

    assert_conforms(code)
  end

  test "repro 77" do
    code = """
    alpha baz: 0x35 do; Config
    ?a
    end ..
    & baz(95.84)(:three) do 2 -> :bar
    spam -> 640
    end

    """

    assert_conforms(code)
  end

  test "repro 78" do
    code = """
    0.\"\"R\
    """

    assert_conforms(code)

    code = """
    0.'!'d\
    """

    assert_conforms(code)
  end

  test "repro 79" do
    code = """
    l[\na]\
    """

    assert_conforms(code)
  end

  test "repro 80" do
    code = """
    ^?\n*a\
    """

    assert_conforms(code)
  end

  test "repro 81" do
    code = """
    not/l\
    """

    assert_conforms(code)

    code = """
    [not/eoe]\
    """

    assert_conforms(code)
  end

  test "repro 82" do
    code = """
    (s;)\
    """

    assert_conforms(code)
  end

  test "repro 83" do
    code = """
    %-e{}\
    """

    assert_conforms(code)
  end

  test "repro 84" do
    # fn end
    code = """
    fn ->;t end\
    """

    assert_conforms(code)
  end

  test "repro 84a" do
    # fn end
    code = """
    fn d->; end\
    """

    assert_conforms(code)
  end

  test "repro 85" do
    code = """
    a do -> end\
    """

    assert_conforms(code)
  end

  test "repro 85a" do
    code = """
    a do (dir;) end\
    """

    assert_conforms(code)
  end

  test "repro 86" do
    code = """
    {(s=s;)}\
    """

    assert_conforms(code)
  end

  test "repro 86a" do
    code = """
    a do x -> ;c end\
    """

    assert_conforms(code)
  end

  test "repro 87" do
    code = """
    a do; @l^h -> 1; end\
    """

    assert_conforms(code)
  end

  test "repro 88" do
    code = """
    d&c do 1 end\
    """

    assert_conforms(code)
  end

  test "repro 89" do
    code = """
    dh^:e do 1 end\
    """

    assert_conforms(code)
  end

  test "repro 90" do
    code = """
    h&A do 1 end\
    """

    assert_conforms(code)
  end

  test "repro 91" do
    code = """
    not a do 1 end |r;\
    """

    assert_conforms(code)
  end

  test "repro 92" do
    code = """
    -!d a do 1 end -!d\
    """

    assert_conforms(code)
  end

  test "repro 93" do
    code = """
    fn a, d\\\\d -> 1 end\
    """

    assert_conforms(code)
  end

  test "repro 94" do
    code = """
    fn a, d: e -> 1 end\
    """

    assert_conforms(code)
  end

  test "repro 95" do
    code = """
    fn a, @d^/d -> 1 end\
    """

    assert_conforms(code)
  end

  test "repro 95a" do
    code = """
    fn a, @d t -> 1 end\
    """

    assert_conforms(code)
  end

  test "repro 95b" do
    code = """
    << d, >>\
    """

    assert_conforms(code)
  end

  test "repro 95c" do
    code = """
    <<a, s: d >>\
    """

    assert_conforms(code)

    code = """
    <<a, 's': d >>\
    """

    assert_conforms(code)

    code = """
    <<a, "s\#{a}": d >>\
    """

    assert_conforms(code)

    code = """
    <<a, "s\#{a <> ""}": d >>\
    """

    assert_conforms(code)
  end

  test "repro 96" do
    code = """
    a(1, s: (+f) )\
    """

    assert_conforms(code)

    code = """
    a 1, (l;)\
    """

    assert_conforms(code)
  end

  test "repro 97" do
    code = """
    \"\#{;}\"\
    """

    assert_conforms(code)

    code = """
    \"\#{a;c*t}\"\
    """

    assert_conforms(code)

    code = """
    ~s/foo\#{;}/\
    """

    assert_conforms(code)

    code = """
    'foo\#{w;r}'\
    """

    assert_conforms(code)

    code = """
    \"\"\"\nfoo\#{;}\n\"\"\"\
    """

    assert_conforms(code)
  end

  test "repro 98" do
    code = """
    %Foo{0}\
    """

    assert_conforms(code)
  end

  test "repro 99" do
    code = """
    fn @b^e -> :ok end\
    """

    assert_conforms(code)
  end

  test "repro 100" do
    code = """
    fn r.\"\"s -> :ok end\
    """

    assert_conforms(code)
  end

  test "repro 101" do
    code = """
    (-n;)\
    """

    assert_conforms(code)
  end

  test "repro 103" do
    code = """
    fn 1 -> i.''l end\
    """

    assert_conforms(code)
  end

  test "repro 104" do
    code = """
    fn |: n -> :ok end\
    """

    assert_conforms(code)

    code = """
    fn asd: n -> :ok end\
    """

    assert_conforms(code)

    code = """
    fn \"asd\": n -> :ok end\
    """

    assert_conforms(code)

    code = """
    fn \"as\#{1}d\": n -> :ok end\
    """

    assert_conforms(code)
  end

  test "repro 105" do
    code = """
    fn a, r<-b -> :ok end\
    """

    assert_conforms(code)
  end

  test "repro 106" do
    code = """
    %0{}\
    """

    assert_conforms(code)

    code = """
    A.et%?.{}\
    """

    assert_conforms(code)

    code = """
    %\"\"{}\
    """

    assert_conforms(code)
  end

  test "repro 107" do
    code = """
    :\"foo\#{}\"\
    """

    assert_conforms(code)
  end

  test "repro 108" do
    code = """
    [\"foo\#{}\": 1]\
    """

    assert_conforms(code)
  end

  test "repro 109" do
    code = """
    foo.\"\"A\
    """

    assert_conforms(code)
  end

  test "repro 110" do
    code = """
    fn s: e -> :ok end\
    """

    assert_conforms(code)
  end

  test "repro 111" do
    code = """
    0.'' do :ok end\
    """

    assert_conforms(code)
  end

  test "repro 112" do
    code = """
    :\"foo\#{\"\"}\"\
    """

    assert_conforms(code)
  end

  test "repro 113" do
    code = """
    foo .'' bar\
    """

    assert_conforms(code)
  end

  test "repro 114" do
    code = """
    fn a, 0<-s -> :ok end\
    """

    assert_conforms(code)
  end

  test "repro 115" do
    code = """
    fn x when n: d -> 1 end\
    """

    assert_conforms(code)

    code = """
    fn x when @d^a<n -> 1 end\
    """

    assert_conforms(code)

    code = """
    fn x when @e&f -> 1 end\
    """

    assert_conforms(code)
  end

  test "repro 116" do
    code = """
    x..not//y\
    """

    assert_conforms(code)
  end

  test "repro 117" do
    code = """
    %Foo{1}\
    """

    assert_conforms(code)
  end

  test "repro 118" do
    code = """
    %!!h{}\
    """

    assert_conforms(code)
  end

  test "repro 119" do
    code = """
    :\"foo\#{}\"\
    """

    assert_conforms(code)
  end

  test "repro 120" do
    code = """
    '''\nfoo\#{n;h}\n'''\
    """

    assert_conforms(code)
  end

  test "repro 121" do
    code = """
    A.\"\"e.{}\
    """

    assert_conforms(code)
  end

  test "repro 122" do
    code = """
    'foo\#{0;d}'\
    """

    assert_conforms(code)
  end

  test "repro 123" do
    code = """
    \"\#{0;d}\"\
    """

    assert_conforms(code)
  end

  test "repro 124" do
    code = """
    ~s\"\"\"\nfoo\#{n;h}\n\"\"\"\
    """

    assert_conforms(code)
  end

  test "repro 125" do
    code = """
    %!0{}\
    """

    assert_conforms(code)
  end

  test "repro 126" do
    code = """
    foo.''d\
    """

    assert_conforms(code)
  end

  test "repro 127" do
    code = """
    %@0{}\
    """

    assert_conforms(code)

    code = """
    %..{}\
    """

    assert_conforms(code)
  end

  test "repro 128" do
    code = """
    A.\"\".{}\
    """

    assert_conforms(code)

    code = """
    A.''.{}\
    """

    assert_conforms(code)
  end

  test "repro 129" do
    code = """
    A.\"\".foo()\
    """

    assert_conforms(code)
  end

  test "repro 130" do
    code = """
    (b);()\
    """

    assert_conforms(code)
  end

  test "repro 131" do
    code = """
    %&0{}\
    """

    assert_conforms(code)
  end

  test "repro 132" do
    code = """
    [\"foo\#{nil}\": 1]\
    """

    assert_conforms(code)

    code = """
    'foo\#{nil}'\
    """

    assert_conforms(code)

    code = """
    \"\#{nil}\"\
    """

    assert_conforms(code)

    code = """
    '''\nfoo\#{nil}\n'''\
    """

    assert_conforms(code)

    code = """
    '''\nfoo\#{nil}\n'''\
    """

    assert_conforms(code)

    code = """
    ~s\"\"\"\nfoo\#{nil}\n\"\"\"\
    """

    assert_conforms(code)

    code = """
    ~s\"\"\"\nfoo\#{nil|s}\n\"\"\"\
    """

    assert_conforms(code)
  end

  test "repro 133" do
    code = """
    def foo ;not+c do 1 end\
    """

    assert_conforms(code)
  end

  test "repro 134" do
    code = """
    %~r//c{}\
    """

    assert_conforms(code)
  end

  test "repro 135" do
    code = """
    %~r//c{}\
    """

    assert_conforms(code)

    code = """
    %~a\"\"{}\
    """

    assert_conforms(code)
  end

  test "repro 136" do
    code = """
    '''\nfoo\#{;nil}edtf|-s}\n'''\
    """

    assert_conforms(code)
  end

  test "repro 137" do
    code = """
    foo[0.'']\
    """

    assert_conforms(code)
  end

  test "repro 138" do
    code = """
    <<a, s: 0.'' >>\
    """

    assert_conforms(code)
  end

  test "repro 139" do
    code = """
    Bar.foo(delta: true, bar: 0o1, qux: 0xA6)(gamma, false, qux: beta, eggs: :bob, delta: -48) do
    ; after
    true
    34.0
    end
    @nil\
    """

    assert_conforms(code)
  end

  test "repro 140" do
    code = """
    &nil
    fn ->
    41.2
    ; end
    \
    """

    assert_conforms(code)
  end

  test "repro 141" do
    code = """
    if true do
    :ok
    end ||| beta; try qux do
    beta -> true
    rescue
    ; after
    gamma
    end
    \
    """

    assert_conforms(code)
  end

  test "repro 142" do
    code = """
    Mod.delta(spam: 21.83, gamma: Schema.Config)() do
    ; end
    baz qux nil, 0\
    """

    assert_conforms(code)
  end

  test "repro 143" do
    code = """
    -707.(-610, eggs) do\n:baz -> 0b11\nrescue\nend\
    """

    assert_conforms(code)
  end

  test "repro 144" do
    code = """
    try gamma do; :foo
    bar
    end\
    """

    assert_conforms(code)
  end

  test "repro 145" do
    code = """
    ... if baz do
    :ok
    end
    bar(Remote, :ok) do
    ; end
    \
    """

    assert_conforms(code)
  end

  test "repro 146" do
    code = """

    foo.bar() do
    8 -> -734
    after
    ; end
    ... .. :ok // nil\
    """

    assert_conforms(code)
  end

  test "repro 147" do
    code = """
    State.Bar.baz(bar: beta, alpha: foo, eggs: foo)(:bob, 93.96, delta: bar, baz: ?x) do
    else
    ; end
    delta.beta ..
    .. // ...
    \
    """

    assert_conforms(code)
  end

  test "repro 148" do
    code = """
    Mod.< bar :ok, 0x80; Foo.delta delta: :three, delta: 934, baz: 305 do
    foo -> false
    delta -> State
    else
    ; 55
    end\
    """

    assert_conforms(code)
  end

  test "repro 148a" do
    code = """
    foo.case true do qux ->
    ?f
    :ok ->


    :foo
    :two -> qux
    else
    ; end
    ; .. | nil\
    """

    assert_conforms(code)
  end

  # =============================================================================
  # Helper function
  # =============================================================================

  defp assert_conforms(code, _opts \\ []) do
    reference = s2q(code)
    actual = toxic_parse(code)

    assert actual == reference,
           """
            AST mismatch for: #{inspect(code)}

           Reference:
           #{inspect(reference, pretty: true)}

           Actual:
           #{inspect(actual, pretty: true)}
           """
  end

  defp s2q(code) do
    Code.string_to_quoted(
      code,
      columns: true,
      token_metadata: true,
      emit_warnings: false
    )
  end

  defp toxic_parse(code) do
    case ToxicParser.parse_string(code, mode: :strict, token_metadata: true) do
      {:ok, result} -> {:ok, result.ast}
      {:error, result} -> {:error, format_error(result)}
    end
  end

  defp format_error(result) do
    case result.diagnostics do
      [%{reason: reason} | _] -> reason
      _ -> :unknown_error
    end
  end
end
