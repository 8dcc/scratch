
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h> /* cosf() */

enum EAudioFormats {
    PCM = 1,
};

enum ENumChannels {
    MONO   = 1,
    STEREO = 2,
};

/* http://soundfile.sapp.org/doc/WaveFormat/ */
typedef struct {
    /* RIFF chunk descriptor */
    char ChunkID[4];
    uint32_t ChunkSize;
    char Format[4];

    /* Format sub-chunk */
    char Subchunk1ID[4];
    uint32_t Subchunk1Size;
    uint16_t AudioFormat;
    uint16_t NumChannels;
    uint32_t SampleRate;
    uint32_t ByteRate;
    uint16_t BlockAlign;
    uint16_t BitsPerSample;

    /* Data sub-chunk */
    char Subchunk2ID[4];
    uint32_t Subchunk2Size;
} wav_header;

int main(void) {
    wav_header h;

    /* We skip h.ChunkSize for now */
    memcpy(h.ChunkID, "RIFF", 4);
    memcpy(h.Format, "WAVE", 4);

    /* Including '\0' */
    memcpy(h.Subchunk1ID, "fmt ", 4);

    /* Size in bytes of next 6 properties */
    h.Subchunk1Size = 16;

    h.AudioFormat = PCM;
    h.NumChannels = MONO;

    /* Samples per second */
    h.SampleRate = 8000;

    /* Size in bits of each channel */
    h.BitsPerSample = 16;

    /* Size in bytes of all the channels in a sample */
    h.BlockAlign = h.NumChannels * (h.BitsPerSample / 8);

    /* Size in bytes of all samples in a second */
    h.ByteRate = h.SampleRate * h.BlockAlign;

    memcpy(h.Subchunk2ID, "data", 4);

    /* Calculate total number of samples depending on our audio duration */
    const int duration_sec = 10;
    const int NumSamples   = h.SampleRate * duration_sec;

    /* Size of the "Data" section */
    h.Subchunk2Size = NumSamples * h.BlockAlign;

    /* Size in bytes of the buffer: Actual sample data + this header */
    h.ChunkSize = h.Subchunk2Size + sizeof(wav_header);

    const int wave_amplitude = 1000;
    const double note_freq   = 256.0;
    uint16_t data[NumSamples];

    /* Populate `data' with `NumSamples' notes of frequency `note_freq' */
    for (int i = 0; i < NumSamples; i++)
        /* Generate cosine wave: https://www.desmos.com/calculator/fuv5xs95i5 */
        data[i] = (uint16_t)(cos((2 * M_PI * note_freq * i) / h.SampleRate) *
                             wave_amplitude);

    FILE* fp = fopen("output.wav", "w");
    fwrite(&h, 1, sizeof(wav_header), fp);
    fwrite(data, sizeof(uint16_t), NumSamples, fp);
    fclose(fp);

    return 0;
}
