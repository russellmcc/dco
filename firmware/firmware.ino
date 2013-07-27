#include "MultiTone.h"
#include "stdint.h"

int kMinFreq = 50;
int kMaxFreq = 5000;
 
int kMinAmp = 12;
int kMaxAmp = 255;
 
void WriteTone(int f)
{
  int amp = 0;
  if(f <= kMinFreq)
    amp = kMinAmp;
  else if(f >= kMaxFreq)
    amp = kMaxAmp;
  else
  {
    int32_t x = f - kMinFreq;   
    x *= (kMaxAmp - kMinAmp);
    x /= (kMaxFreq-kMinFreq);
    amp = kMinAmp + x;
  }
  analogWrite(A0, amp);
  MultiTone(13, f);
}
 
void setup()
{
  WriteTone(700);
}

extern "C" int analogRead12(unsigned char pin);
 
void loop()
{
  int input = analogRead12(8);
  // Okay, we're scaling by about .64, we have a 3.3 volt range and 12 bits.
  // Therefore, we have 1251 counts per unscaled volt which corresponds to 
  // 803 counts per scaled volt.
  float fInput = input;
  fInput /= 803;
  float scale = pow(2, fInput);
  float frequency = scale * kMinFreq;
  
  if(frequency < 20)
    frequency = 20;
  if(frequency > 15000)
    frequency = 15000;
    
  WriteTone(frequency);
}
