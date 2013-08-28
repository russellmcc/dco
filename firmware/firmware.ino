#include "MultiTone.h"
#include "stdint.h"
#include "table.h"

uint32_t running_average; //1.15 format

// optional lowpass filter
const float alphaFloat = 0.7;
const uint32_t alpha = 0xFFFF * alphaFloat; //0.16
const uint32_t oneMinusAlpha = 0xFFFF * (1 - alphaFloat); //0.16

// we need 12 bits of ADC
extern "C" int analogRead12(unsigned char pin);

// for prescale hysteresis
int16_t lastSwapInput = 0xFFFF;
int16_t lastSwapPrescale = 100;
const uint16_t swapThreshold = 20;

void EnableDFLL()
{
  // enable the 32KHz clock
  OSC.CTRL |= OSC_RC32KEN_bm;

  // wait for stability
  while(not (OSC.STATUS & OSC_RC32KRDY_bm));

  // enable DFLL for extra power drain and accuracy.
  OSC.DFLLCTRL = (OSC.DFLLCTRL & ~OSC_RC32MCREF_gm) | OSC_RC32MCREF_RC32K_gc;
  DFLLRC32M.CTRL |= DFLL_ENABLE_bm;
  
  // wait for 32MHz stability.
  while(not (OSC.STATUS & OSC_RC32MRDY_bm));
}

void SetupTimers()
{
  // TCC1 is the prescaler timer, triggering events on EVCH6
  TCC1.CTRLA = TC_CLKSEL_DIV1_gc;
  TCC1.CTRLB = (TCC1.CTRLB & ~TC0_WGMODE_gm) | TC_WGMODE_SINGLESLOPE_gc;
  TCC1.PERBUF = 0x3; // every fourth clock
  
  // set it up to trigger EVCH6 events whenever TCC1 overflows
  EVSYS.CH6MUX = EVSYS_CHMUX_TCC1_OVF_gc;
  
  // TCC0D is the output timer (pin D13)
  pinMode(13, OUTPUT);
  TCC0.CTRLA = TC_CLKSEL_EVCH6_gc;
  TCC0.CTRLB = (TCC0.CTRLB & ~TC0_WGMODE_gm) | TC_WGMODE_SINGLESLOPE_gc;
  TCC0.CTRLB |= TC0_CCDEN_bm;
  TCC0.PERBUF = 0x8FFF; // half-way
  TCC0.CCDBUF = 10;
}

void setup()
{
  // use the arduino library calls to 
  // get everything into the right mode
  // we'll later use the setup done here
  // to just do fast writes to the underlying registers.
  analogWrite(A0, 12);
  EnableDFLL();
  SetupTimers();
  running_average = analogRead12(8) << 3; //1.15
}

void UpdateTimers(uint16_t input)
{
  uint16_t preScale = pgm_read_word(&kHi[input]);
  uint16_t period = pgm_read_word(&kLo[input]);
  if(preScale != lastSwapPrescale)
  {
    lastSwapInput = input;
    lastSwapPrescale = preScale;
  }
  // wait until all timers are stabilized
  while ((TCC0.CTRLGSET & (TC0_PERBV_bm | TC0_CCDBV_bm)) &&
         (TCC1.CTRLGSET & (TC1_PERBV_bm))){}
  TCC0.PERBUF = period;
  TCC0.CCDBUF = period >> 2; // for some reason we have to set this every time?*/
  TCC1.PERBUF = preScale;
}

void loop()
{
  uint16_t input = analogRead12(8); //4.12
  running_average = alpha * running_average >> 16;
  running_average += (oneMinusAlpha * (input << 3)) >> 16;
  input = running_average >> 3;
  input &= 0xFFF; // 12 bit.

  // A0 is DACB CH1
  if ( (DACB.STATUS & DAC_CH1DRE_bm) )
    DACB_CH1DATA = pgm_read_word(&kAmps[input]) & 0x0FFF;
  
  uint16_t diffFromLastSwap = input > lastSwapInput ?
                              input - lastSwapInput :
                              lastSwapInput - input;
  
  uint16_t newPrescale = pgm_read_word(&kHi[input]);
  if((newPrescale == lastSwapPrescale) || 
     (diffFromLastSwap > swapThreshold))
    UpdateTimers(input);
    
}
