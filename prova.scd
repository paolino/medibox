(
var timeNow,t=TempoClock.default;
t.tempo = 125/60;    // 2 beats/sec, or 120 BPM
timeNow = TempoClock.default.beats;
"Time is now: ".post; timeNow.postln;
"Scheduling for: ".post; (timeNow + 5).postln;
t.schedAbs(timeNow + 5,
    { "Time is later: ".post; thisThread.clock.beats.postln; nil });
)


(
r = Routine({
    var delta;
    loop {
        delta = rrand(1, 3) * 0.5;
        "Will wait ".post; delta.postln;
		b.set (0.5 + (delta/8));
		c.set (0.3 + (delta/8));
        delta.yield;
    }
});
)
r.next;

TempoClock.default.sched(0, r);

r.stop;







{ [SinOsc.ar(110, 0, 0.1), SinOsc.ar(220, 0, 0.1)] }.play;


s.boot;
(
b=Bus.control(s);
c=Bus.control(s);
d=Bus.control(s);
l=[Bus.control(s),Bus.control(s),Bus.control(s),Bus.control(s)];
p=[Bus.control(s),Bus.control(s),Bus.control(s),Bus.control(s)];
z=[Bus.control(s),Bus.control(s),Bus.control(s),Bus.control(s)];
)
(
~signal = {
	var s1,r1,r2,zot,fu;
	fu = Lag.kr(midicps (floor (cpsmidi (b.kr*100))),0.05);
	zot = LFSaw.ar(fu * (1 + ([l[0].kr,l[1].kr,l[2].kr,l[3].kr]/100)
	),pi*[z[0].kr,z[1].kr,z[2].kr,z[3].kr]);
	s1 = Mix.ar (SinOsc.ar (
		fu * (1 + (SinOsc.kr(20)/100)),
		((pi*zot*[p[0].kr,p[1].kr,p[2].kr,p[3].kr]/4)).mod(2*pi),
		0.5,
		0.1)
		,0);

	r1 = LPF.ar(s1,fu *12 ,1);
	r2 = HPF.ar(r1,fu*2,1);
	Pan2.ar(Lag.kr(c.kr,0.01)*r2)
	}
)
~signal.stop

8
x=Synth(\basso,1004);
x.set(\amp,1)
x.set(\freq,0.5)
x.free

~x.index;
~x.bus
~x.monitor.stop
~x.stop;
~x = {	RLPF.ar(Impulse.arp=ProxySpace.push(s);
~signal.play;
~signal.index
~signal
~signal.fadeTime_(0.1) // fading in and out in 2 secs([5, 7]) * 5, [1800, 2000], 0.8) }
x.free;
x = Synth(\pulf);





	~signal= {SinOsc.ar(400, 0, 1)!2}


~signal= {SinOsc.ar([400, 404], 0, LFNoise0.kr(4))}


~signal= {Saw.ar([400, 404],  LFNoise0.kr(4))}
~signal = {|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,vol=0.4|
    var clk, pan,gen,env;
    clk = Impulse.ar(trate*200,sw);
    pan = WhiteNoise.ar(0.1);
	//env = Env.perc(0.001, 1, 1, -8) ;
	//gen = EnvGen.kr(env, gate, doneAction: 2);
		LPF.ar(TGrains.ar(2, clk, 3, 0.5 +freq, ((pos + pos2/10)*BufDur.kr(3))  + (amp/10/BufDur.kr(3)* LFSaw.ar(trate*200)), ((2 + (dur*48))/1000), pan, vol, 512),500)
}

~signal= {Saw.ar([400, 404],  Pulse.ar(2))}


~signal= {Saw.ar([400, 404],  Pulse.ar(Line.kr(1, 30, 20)))}


~signal= {LFSaw.ar([400, 404],  LFNoise0.kr(4))}


~signal= {Pulse.ar([400, 404],  LFNoise0.kr(4))}


~signal= {Blip.ar([400, 404],  12, Pulse.ar(2))}


~signal= {Blip.ar([400, 404],  24, LFNoise0.kr(4))}


~signal= {Blip.ar([400, 404],  4, LFNoise0.kr(4))}


~signal= {Blip.ar([400, 404],  MouseX.kr(4, 40), LFNoise0.kr(4))}
~signal = {|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,vol=0.4|
    var clk, pan,gen,env;
    clk = Impulse.ar(trate*200,sw);
    pan = WhiteNoise.ar(0.1);
	//env = Env.perc(0.001, 1, 1, -8) ;
	//gen = EnvGen.kr(env, gate, doneAction: 2);
		LPF.ar(TGrains.ar(2, clk, 3, 0.5 +freq, ((pos + pos2/10)*BufDur.kr(3))  + (amp/10/BufDur.kr(3)* LFSaw.ar(trate*200)), ((2 + (dur*48))/1000), pan, vol, 512),500)
}

~signal= {Blip.ar([200, 204],  5, Pulse.ar(1))}


~signal[1] = \filter -> {arg sig; (sig*0.6)+FreeVerb.ar(sig, 0.85, 0.86, 0.3)}; // reverb

~signal[2] = \filter -> {arg sig; sig + AllpassC.ar(sig, 1, 0.15, 1.3 )}; // delay

~signal[3] = \filter -> {arg sig; (sig * SinOsc.ar(2.1, 0, 5.44, 0))*0.5}; // tremolo

~signal[4] = \filter -> {arg sig; PitchShift.ar(sig, 0.008, SinOsc.ar(2.1, 0, 0.11, 1))}; // pitchshift

~signal[5] = \filter -> {arg sig; (3111.33*sig.distort/(1+(2231.23*sig.abs))).distort*0.2}; // distort

~signal[1] = nil

~signal[2] = nil

~signal[3] = nil

~signal[4] = nil

~signal[5] = nil




~z.play; // monitor. this starts also the synth, if the group wasn't playing.

~z = { SinOsc.ar(ExpRand(20, 660) ! 2) * Saw.ar(ExpRand(200, 960) ! 2) * 0.1 };
~z.index
~z.release; // release the synth. the current fadeTime is used for fade out

~z.send; // send a new synth. the current fadeTime is used for fade in

~z.send; // send another synth, release the old

~z.release;


~z.stop;



(
SynthDef(\pul,{|rel=0.5,freq,fat=0.2,dur,ampl=0.5|
	var env,gen,s1,s2,s3,r1,r2,fu,zot,env2,gen2;
	fu = midicps (freq);
	env = Env.perc(0.01, dur/2, ampl,-10);
	gen = EnvGen.kr(env, 1, doneAction: 0);
	zot = LFSaw.ar(fu * (1 + ([l[0].kr,l[1].kr,l[2].kr,l[3].kr]/100)
	),pi*[z[0].kr,z[1].kr,z[2].kr,z[3].kr]);
	s1 = Mix.ar (SinOsc.ar (
		fu * (1 +  (SinOsc.ar(fu) /100)),
		((pi*zot*[p[0].kr,p[1].kr,p[2].kr,p[3].kr]/4)).mod(2*pi),
		0.5,
		0.1)
		,0);
	env2 = Env.perc(0.09, dur, ampl,-1);
	gen2 = EnvGen.kr(env2, 1, doneAction: 2);
	r1 = LPF.ar(s1,fu *12 ,1);
	r2 = HPF.ar(r1,fu*2,1);
	Out.ar(0,Pan2.ar((gen*r1) + (gen2*r2)))
	}
).add
)
MIDIIn.connectAll;
(
MIDIFunc.cc({arg ...args; b.set (args[0]/128);args.postln}, 0,1); // match cc 1.
MIDIFunc.cc({arg ...args; c.set (args[0]/128);args.postln}, 1,1); // match cc 1.
MIDIFunc.cc({arg ...args; d.set (args[0]/128);args.postln}, 2,1); // match cc 1.
MIDIFunc.cc({arg ...args; l[0].set (args[0]/128);args.postln}, 8,1); // match cc 1.
MIDIFunc.cc({arg ...args; l[1].set (args[0]/128);args.postln}, 9,1); // match cc 1.
MIDIFunc.cc({arg ...args; l[2].set (args[0]/128);args.postln}, 10,1); // match cc 1.
MIDIFunc.cc({arg ...args; l[3].set (args[0]/128);args.postln}, 11,1); // match cc 1.
MIDIFunc.cc({arg ...args; p[0].set (args[0]/128);args.postln}, 12,1); // match cc 1.
MIDIFunc.cc({arg ...args; p[1].set (args[0]/128);args.postln}, 13,1); // match cc 1.
MIDIFunc.cc({arg ...args; p[2].set (args[0]/128);args.postln}, 14,1); // match cc 1.
MIDIFunc.cc({arg ...args; p[3].set (args[0]/128);args.postln}, 15,1); // match cc 1.
MIDIFunc.cc({arg ...args; z[0].set (args[0]/128);args.postln}, 16,1); // match cc 1.
MIDIFunc.cc({arg ...args; z[1].set (args[0]/128);args.postln}, 17,1); // match cc 1.
MIDIFunc.cc({arg ...args; z[2].set (args[0]/128);args.postln}, 18,1); // match cc 1.
MIDIFunc.cc({arg ...args; z[3].set (args[0]/128);args.postln}, 19,1); // match cc 1.
)
c = Bus.control(s)

midicps(16)
(
Pbind(\instrument, \pul,
	\dur, Pseq([12,24,24,24,36,24,24,24]/100,inf),
	\freq,Pseq([36,36,33,37,24,26,27,31]+12, inf),
	\ampl,Pseq([0.3,0.2,0.3,0.4,0.6,0.5,0.4,0.3],inf)
)
.play;
)
	\l2,Pseq([0.2,0.4,0.2,0.2,0.3,0.3,0.2,0.3]*160,inf),
		\p2, Pseq([0.5,0.75,1,0.5,0.3,1,0.5,0.3]*0.3, inf),
	\l3,Pseq([0.2,0.4,0.2,0.2,0.3,0.3,0.2,0.3]*120,inf),
		\p3, Pseq([0.5,0.75,1,0.5,0.3,1,0.5,0.3]*0.1, inf),
	\l4,Pseq([0.2,0.4,0.2,0.2,0.3,0.3,0.2,0.3]*60,inf),
		\p4, Pseq([0.5,0.75,1,0.5,0.3,1,0.5,0.3]*0.1, inf),
Pbind(\instrument, \pul, \freq, Pseq([0.5,0.75,1,0.5,0.3,1,0.5,0.3], inf), \dur, 0.48,\s1a,Pseq([0.2,0.4,0.2,0.2,0.3,0.3,0.2,0.3]/2,inf),\s2a,Pseq([0.2,0.4,0.2,0.2,0.3,0.3,0.2,0.3]/2,inf),\rel,Pseq([0.2,0.1,0.2,0.2,0.3,0.3,0.2,0.3]/2,inf)).play;

(
SynthDef(\bass1,{|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,s1a=0.3,s2a=0.3,s3a=0.3,tag=0.5,det = 0,rq=1,rel=0|

    var clk, pan,gen,env;
	var s1,s2,s3,s4,r1,r2,gen3,env2;
    //clk = Impulse.ar(trate*100);
    pan = SinOsc.kr(8)/30;
	env = Env.perc(0.01, rel, 1);
	gen = EnvGen.kr(env, 1, doneAction: 2);
	env2 = Env.perc(0.05, 0.12 , 1);
	gen3 = EnvGen.kr(env2, 1);

	s1 = s1a * LFPulse.ar (40 + (freq * 40),LFSaw.kr(2)*0.1);
	s3 = s3a * LFPulse.ar ((40 + (freq * 40.2)),LFSaw.kr(1)*0.1);
	s2 = gen3*s2a * LFSaw.ar ((40 + (freq * 40))*2,LFSaw.kr(1)*0.1);


	r1 = RLPF.ar(
		s1 + s3  + s2,
		((SinOsc.kr(freq*20) * 0.4) + 2) * (freq * 160),
		((SinOsc.kr(freq*30) * 0.4) + (2))
	);
	r2 =RHPF.ar(r1,40,1,1);

	Out.ar(0,gen*Pan2.ar(r2,pan))
}).add;
)
MIDIIn.connectAll;
(
a=Synth(\bass1);

MIDIFunc.cc({arg ...args; ~signal.set (\freq,args[0]/128);args.postln}, 0); // match cc 1.
MIDIFunc.cc({arg ...args; ~signal.set (\tag,args[0]/128);args.postln}, 1); // match cc 1
MIDIFunc.cc({arg ...args; ~signal.set (\s1a,args[0]/128);args.postln}, 2); // match cc 1
MIDIFunc.cc({arg ...args; ~signal.set (\s2a,args[0]/128);args.postln}, 3); // match cc 1
MIDIFunc.cc({arg ...args; ~signal.set (\det,args[0]/128);args.postln}, 4); // match cc 1
MIDIFunc.cc({arg ...args; ~signal.set (\rq,args[0]/128);args.postln}, 5); // match cc 1
MIDIFunc.cc({arg ...args; ~signal.set (\pos2,args[0]/128);args.postln}, 6); // match cc 1

)
a=Synth(\bass1);
s.boot;
s.sendMsg(\b_allocRead, 3, "/home/paolino/WAV/ByType/Percussion/mono/7.wav");
// get a bus
b = Bus.control(s);
a = Synth(\help_notRand);
(
SynthDef(\micro,{|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,vol=0.4|
    var clk, pan,gen,env;
    clk = Impulse.ar(trate*100);
    pan = WhiteNoise.ar(0.1);
	//env = Env.perc(0.001, 1, 1, -8) ;
	//gen = EnvGen.kr(env, gate, doneAction: 2);
	Out.ar(0,TGrains.ar(2, clk, 3, 0.5 +freq, ((pos + pos2/10)*BufDur.kr(3))  + (amp/10/BufDur.kr(3)* LFSaw.ar(sw*100)),((2 + (dur*48))/1000) , pan, vol))
}).add;
)
p = ProxySpace.push(s)


~signal = {|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,vol=0.4,freqdur,phadur,ampdur,freqpos,phapos,amppos|
    var clk, pan,gen,env;
		clk = Gate.ar(Impulse.ar(trate*100),LFSaw.ar(2)-0.5);
    pan = WhiteNoise.ar(0.3);
	//env = Env.perc(0.001, 1, 1, -8) ;
	//gen = EnvGen.kr(env, gate, doneAction: 2);
		Pan2.ar(Mix.ar (LPF.ar (TGrains.ar(2, clk, 3, 0.5 +freq, ((1 + (WhiteNoise.ar()/2))*BufDur.kr(3))  + (amp*BufDur.kr(3)* (0.5 + (LFSaw.ar(freqpos,phapos)*amppos))),BufDur.kr(3)*dur * (1 + (LFSaw.ar(freqdur,phadur)*ampdur)), pan, vol, 512),5000)))
}

~signal = {|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,vol=0.4,freqdur,phadur,ampdur,freqpos,phapos,amppos|
    var clk, pan,gen,env;
		clk = Gate.ar(Impulse.ar(trate*100),LFSaw.ar(2)-0.5);
    pan = WhiteNoise.ar(0.3);
	//env = Env.perc(0.001, 1, 1, -8) ;
	//gen = EnvGen.kr(env, gate, doneAction: 2);
		Pan2.ar(Mix.ar (LPF.ar (TGrains.ar(2, clk, 3, 0.5 +freq, ((pos + pos2/10)*BufDur.kr(3))  + (amp*BufDur.kr(3)* (0.5 + (LFSaw.ar(freqpos,phapos)*amppos))),BufDur.kr(3)*dur * (1 + (LFSaw.ar(,phadur)*ampdur)), pan, vol, 512),5000)))
}
~signal.play
~signal.stop
;
)

(
p = Pbind(
    \type, \set,    // This tells it we'll be setting parameters of an existing node...
    \id, ~signal,    // ...this tells it whose parameters we'll be setting
    \args, #[\freq, \vol],  // and this tells it which parameters to set
    \freq, Pwhite(0.4, 0.8,inf),
    \dur, 0.2,
    \vol, Pseq((0.5,0.45 .. 0.1), inf)
).play;
)
~signa
p.stop
x.free

MIDIIn.connectAll;
	(

		MIDIFunc.cc({arg ...args; ~signal.set (\freq,args[0]/128);args.postln}, 0); // match cc 1.
		MIDIFunc.cc({arg ...args; ~signal.set (\dur,args[0]/128);args.postln}, 1); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\trate,args[0]/128);args.postln}, 2); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\sw,args[0]/128);args.postln}, 3); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\amp,args[0]/128);args.postln}, 4); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\pos,args[0]/128);args.postln}, 5); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\pos2,args[0]/128);args.postln}, 6); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\vol,args[0]/128);args.postln}, 7); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\freqdur,args[0]/128);args.postln}, 8); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\phadur,args[0]/128);args.postln}, 9); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\ampdur,args[0]/128);args.postln}, 10); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\freqppos,args[0]/128);args.postln}, 11); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\phapos,args[0]/128);args.postln}, 12); // match cc 1
		MIDIFunc.cc({arg ...args; ~signal.set (\amppos,args[0]/128);args.postln}, 13); // match cc 1

	)

(
SynthDef(\ixibass, {arg out=0, amp=0.3, t_trig=1, freq=100, rq=0.004;
var env, signal;
var rho, theta, b1, b2;
b1 = 1.98 * 0.989999999 * cos(0.09);
b2 = 0.998057.neg;
signal = SOS.ar(K2A.ar(t_trig), 0.123, 0.0, 0.0, b1, b2);
signal = RHPF.ar(signal, freq, rq) + RHPF.ar(signal, freq*0.5, rq);
signal = Decay2.ar(signal, 0.4, 0.3, signal);
DetectSilence.ar(signal, 0.01, doneAction:2);
Out.ar(out, signal*(amp*0.45)!2);

}).add;
)

(
Pmono(\ixibass,
               \dur, Pseq([0.25, 0.25, 0.5, 0.75], inf),
               \midinote, Pseq([ 60, 50, 48, 62] , inf)).play;

Pmono(\ixibass,
               \dur, Pseq([0.25, 0.25, 0.5, 0.25, 0.25, 0.25], inf),
               \midinote, Pseq([ 48, 45, 48, 55, 57, 60] , inf)).play(quant:[0,3.5,0])
)



Synth(\ixibass, [\freq, 110])



