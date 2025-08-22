from manim import *
from helper import Array


class BinaryArithmetic(Scene):
    def construct(self):
        title = Text("Binary Arithmetic").scale(0.8).to_edge(UP)
    
        barith = VGroup(
            VGroup(
                Text("Addition").scale(0.9),
                MathTex("0+0=0"),
                MathTex("1+0=1"),
                MathTex("1+1=10"),
                MathTex("1+1+1=11"),
            ).arrange(DOWN, aligned_edge=LEFT),
            VGroup(
                Text("Subtraction").scale(0.9),
                MathTex("0-0=0"),
                MathTex("1-0=1"),
                MathTex("1-1=0"),
                MathTex(r"0-1=\text{ borrow }1\longrightarrow10-1=1"),
            ).arrange(DOWN, aligned_edge=LEFT),
            VGroup(
                Text("Multiply").scale(0.9),
                MathTex(r"0\times0=0"),
                MathTex(r"1\times0=0"),
                MathTex(r"1\times1=1"),
            ).arrange(DOWN, aligned_edge=LEFT)
        ).scale(0.7).arrange(RIGHT, buff=1).next_to(title, DOWN, buff=0.5)
        self.add(title, barith)


class HexArithmetic(Scene):
    def construct(self):
        title = Text("Hex Arithmetic").scale(0.8).to_edge(UP)
    
        harith = VGroup(
            VGroup(
                VGroup(
                    Text("Addition").scale(0.9),
                    MathTex(r"9+1=\text{A}"),
                    MathTex(r"\text{B}+\text{D}=18"),
                    MathTex(r"\longrightarrow11+13=18"),
                    MathTex(r"\longrightarrow\text{result }24\;%\;16=8"),
                    MathTex(r"\longrightarrow\text{carry }24\;/\;16=1"),
                ).arrange(DOWN, aligned_edge=LEFT),
                VGroup(
                    Text("Subtraction").scale(0.9),
                    MathTex(r"\text{A}-1=9"),
                    MathTex(r"\text{F}(15)-\text{C}(12)=3"),
                    MathTex(r"\text{B}-\text{E}=\text{D}"),
                    MathTex(r"\longrightarrow\text{borrow 1, }\text{B}=11+16=27"),
                    MathTex(r"\longrightarrow\text{B}-\text{E}=27-14=13\text{(D)}"),
                ).arrange(DOWN, aligned_edge=LEFT),
            ).arrange(RIGHT, buff=2),
            VGroup(
                Text("Multiply").scale(0.9),
                MathTex(r"\text{A}\times\text{D}=82"),
                MathTex(r"\longrightarrow\text{result }10\times13=130\;\%\;16=2"),
                MathTex(r"\longrightarrow\text{carry }130\;/\;16=8"),
            ).arrange(DOWN, aligned_edge=LEFT),
        ).scale(0.7).arrange(DOWN, buff=0.6).next_to(title, DOWN, buff=0.5)
        self.add(title, harith)


class DecimalToBase(Scene):
    def construct(self):
        title = Text("Decimal To Hex").scale(0.8).to_edge(UP)

        whole = 100
        frac = 0.36
        base = 16
        places = 8
        scale = 0.8

        weq = VGroup()
        feq = VGroup()
        wres = []
        fres = []
        while whole > 0:
            q, r = divmod(whole, base)
            weq.add(MathTex(f"{whole}", "=", f"{base}", r"\times", f"{q}", "+", f"{format(r,'X')}"))
            wres.insert(0, format(r,'X'))
            whole = q
        wres = "".join(wres)

        while frac > 0.0 and places > 0:
            fb = round(frac * base, 2)
            fi = int(fb)
            feq.add(MathTex(f"{frac}", r"\times", f"{base}", "=", f"{fb}", rf"\text{{{format(fi,'X')}}}"))
            fres.append(format(fi,'X'))
            frac = round(fb - fi, 2)
            places = places - 1
        fres = "".join(fres)

        x, y, z, buff = -4.5, 1.6, 0, 0.5
        for idx, eq in enumerate(weq.scale(scale)):
            eq[0].move_to([x, y - idx * buff, z], aligned_edge=RIGHT)
            eq[1:].next_to(eq[0], RIGHT)
            eq[4].next_to(eq[3], RIGHT, buff=0.3, aligned_edge=RIGHT)
            eq[5:].move_to([x + 2.1, y - idx * buff, z])

        arrup = Arrow(
            start=weq.get_bottom(),
            end=weq.get_top(),
            tip_length=0.2,
            stroke_width=2,
            buff=0,
        ).next_to(weq, RIGHT, buff=0.4)

        wreseq = (
            MathTex(rf"\text{{({wres})}}_{{{base}}}")
            .scale(0.8)
            .next_to(weq, DOWN, buff=0.4)
            .shift(RIGHT * 0.4)
        )

        x, y, z, buff = 1, 1.6, 0, 0.5
        for idx, eq in enumerate(feq.scale(scale)):
            eq[0].move_to([x, y - idx * buff, z], aligned_edge=RIGHT)
            eq[1:].next_to(eq[0], RIGHT)
            eq[5].move_to([x + 2.9, y - idx * buff, z], aligned_edge=LEFT)

        s = VGroup(
            SurroundingRectangle(feq[1][4], stroke_width=2),
            SurroundingRectangle(feq[1][5], color=RED_E, stroke_width=2),
            SurroundingRectangle(feq[2][0], stroke_width=2)
        )

        arrdwn = Arrow(
            start=feq.get_top(),
            end=feq.get_bottom(),
            tip_length=0.2,
            stroke_width=2,
            buff=0,
        ).next_to(feq, RIGHT, buff=0.4)

        freseq = (
            MathTex(rf"\text{{(0.{fres})}}_{{{base}}}")
            .scale(0.8)
            .next_to(feq, DOWN, buff=0.4)
            .shift(RIGHT * 0.3)
        )

        res = MathTex(rf"100.36 = \text{{({wres}.{fres})}}_{{{base}}}").scale(0.8).next_to(title, DOWN, buff=0.4)

        self.add(title, weq, wreseq, arrup, feq, freseq, arrdwn, s, res)
        self.wait(3)


class BaseToDecimal(Scene):
    def construct(self):
        title = Text("Binary To Decimal").scale(0.8).to_edge(UP)
        res = MathTex("(1100100.01011100)_{2} = (100.36)_{{{10}}}").scale(0.8).next_to(title, DOWN, buff=0.5)
        arr = VGroup(
            Array(data=[6,5,4,3,2,1,0,'',-1,-2,-3,-4,-5,-6,-7,-8],index=False,cell_width=0.7,fs=20, stroke_width=0),
            Array(data=list("1100100.01011100"),index=False,cell_width=0.7,fs=28),
        ).arrange(DOWN, buff=0, aligned_edge=LEFT).next_to(res, DOWN, buff=0.5)
        arr[1].highlight_cell(self, 0,1,4,9,11,12,13,fill=GREEN_E,animate=False)
        exp = VGroup(
            MathTex(r"= 1\cdot2^6+2^5+2^2+2^{-2}+2^{-4}+2^{-5}+2^{-6}"),
            MathTex(r"\approx 100.36")
        ).scale(0.9).arrange(DOWN, aligned_edge=LEFT).next_to(arr, DOWN, aligned_edge=LEFT, buff=0.5)
        self.add(title, res, arr, exp)
        self.wait(2)

        self.play(*[FadeOut(mob) for mob in self.mobjects])
        self.wait(1)

        title = Text("Octal To Decimal").scale(0.8).to_edge(UP)
        res = MathTex("(144.270)_{8} = (100.36)_{{{10}}}").scale(0.8).next_to(title, DOWN, buff=0.5)
        arr = VGroup(
            Array(data=[2,1,0,'',-1,-2,-3],index=False,cell_width=0.7,fs=20, stroke_width=0),
            Array(data=list("144.270"),index=False,cell_width=0.7,fs=28),
        ).arrange(DOWN, buff=0, aligned_edge=LEFT).next_to(res, DOWN, buff=0.5)
        exp = VGroup(
            MathTex(r"= 1\cdot8^2+4\cdot8+4+2\cdot8^{-1}+7\cdot8^{-2}"),
            MathTex(r"\approx 100.36")
        ).scale(0.9).arrange(DOWN, aligned_edge=LEFT).next_to(arr, DOWN, aligned_edge=LEFT, buff=0.5)
        self.add(title, res, arr, exp)
        self.wait(2)

        self.play(*[FadeOut(mob) for mob in self.mobjects])
        self.wait(1)

        title = Text("Hex To Decimal").scale(0.8).to_edge(UP)
        res = MathTex(r"\text{(64.5C)}_{{{16}}} = (100.36)_{{{10}}}").scale(0.8).next_to(title, DOWN, buff=0.5)
        arr = VGroup(
            Array(data=[1,0,'',-1,-2],index=False,cell_width=0.7,fs=20, stroke_width=0),
            Array(data=list("64.5C"),index=False,cell_width=0.7,fs=28),
        ).arrange(DOWN, buff=0, aligned_edge=LEFT).next_to(res, DOWN, buff=0.5)
        exp = VGroup(
            MathTex(r"= 6\cdot16+4+5\cdot16^{-1}+\text{C}(12)\cdot16^{-2}"),
            MathTex(r"\approx 100.36")
        ).scale(0.9).arrange(DOWN, aligned_edge=LEFT).next_to(arr, DOWN, aligned_edge=LEFT, buff=0.5)
        self.add(title, res, arr, exp)
        self.wait(3)


class BinaryToOctHex(Scene):
    def construct(self):
        title = Text("Binary To Octal").scale(0.8).to_edge(UP)
        res = MathTex("(1100100.01011100)_{2} = (144.270)_{8}").scale(0.8).next_to(title, DOWN, buff=0.5)
        arr = Array(data=list("001100100.010111000"),index=False,cell_width=0.7,fs=28,stroke_width=0).next_to(res, DOWN, buff=0.5)
        arr.get_label(0).set_color(RED)
        arr.get_label(1).set_color(RED)
        arr.get_label(-1).set_color(RED)

        brace = VGroup(
            Brace(VGroup(arr.get_label(0),arr.get_label(1),arr.get_label(2)),DOWN),
            Brace(VGroup(arr.get_label(3),arr.get_label(4),arr.get_label(5)),DOWN),
            Brace(VGroup(arr.get_label(6),arr.get_label(7),arr.get_label(8)),DOWN),
            Brace(VGroup(arr.get_label(10),arr.get_label(11),arr.get_label(12)),DOWN),
            Brace(VGroup(arr.get_label(13),arr.get_label(14),arr.get_label(15)),DOWN),
            Brace(VGroup(arr.get_label(16),arr.get_label(17),arr.get_label(18)),DOWN),
        )

        brace_text = VGroup(
            brace[0].get_text("1"),
            brace[1].get_text("4"),
            brace[2].get_text("4"),
            brace[3].get_text("2"),
            brace[4].get_text("7"),
            brace[5].get_text("0"),
        )

        self.add(title, res, arr, brace, brace_text)
        self.wait(2)

        self.play(*[FadeOut(mob) for mob in self.mobjects])
        self.wait(1)

        title = Text("Binary To Hex").scale(0.8).to_edge(UP)
        res = MathTex(r"(1100100.01011100)_{2} = \text{(64.5C)}_{{{16}}}").scale(0.8).next_to(title, DOWN, buff=0.5)
        arr = Array(data=list("01100100.01011100"),index=False,cell_width=0.7,fs=28,stroke_width=0).next_to(res, DOWN, buff=0.5)
        arr.get_label(0).set_color(RED)

        brace = VGroup(
            Brace(VGroup(arr.get_label(0),arr.get_label(1),arr.get_label(2), arr.get_label(3)),DOWN),
            Brace(VGroup(arr.get_label(4),arr.get_label(5),arr.get_label(6), arr.get_label(7)),DOWN),
            Brace(VGroup(arr.get_label(9),arr.get_label(10),arr.get_label(11), arr.get_label(12)),DOWN),
            Brace(VGroup(arr.get_label(13),arr.get_label(14),arr.get_label(15), arr.get_label(16)),DOWN),
        )

        brace_text = VGroup(
            brace[0].get_text("6"),
            brace[1].get_text("4"),
            brace[2].get_text("5"),
            brace[3].get_text("C"),
        )

        self.add(title, res, arr, brace, brace_text)
        self.wait(3)
