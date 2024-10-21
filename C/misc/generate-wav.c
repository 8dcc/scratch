/*
 * Copyright 2024 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 *
 * ----------------------------------------------------------------------------
 *
 * TODO: Test in big-endian systems.
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h> /* cosf() */

/*
 * Audio formats. Normally Pulse-Code Modulation (PCM), where each sample is
 * quantized to the nearest value within a range of digital steps.
 *
 * Values other than 1 indicate some form of compression.
 */
enum EAudioFormats {
    PCM = 1,
};

/*
 * Number of channels. I think the difference between "Mono" and "Stereo" is
 * pretty clear.
 */
enum ENumChannels {
    MONO   = 1,
    STEREO = 2,
};

/*
 * Structure of a WAV header. For more information, see `init_wav_header'
 * function below.
 */
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
} WavHeader;

/*
 * Initialize a `WavHeader' structure with the specified parameters.
 *
 * Credits: http://soundfile.sapp.org/doc/WaveFormat/
 */
static void init_wav_header(WavHeader* h, enum ENumChannels num_channels,
                            int sample_rate, int audio_len_secs) {
    /*
     * First, the "RIFF" chunk descriptor.
     *
     * The format is WAVE, which requires two sub-chunks, "fmt " and
     * "data". Notice how the first sub-chunk ends with a space ' ' character.
     *
     * We will skip the `ChunkSize' for now; it will be set last.
     */
    memcpy(h->ChunkID, "RIFF", 4);
    memcpy(h->Format, "WAVE", 4);

    /*
     * We start with the "fmt " sub-chunk. Used to describe the format of the
     * sound information in the "data" sub-chunk.
     *
     * After the sub-chunk ID, we specify the size in bytes of the next 6
     * properties:
     *
     *   (16 + 16 + 32 + 32 + 16 + 16) / 8 = 16
     *
     * We specify the audio format, in this case hard-coded to PCM.
     *
     * We specify the number of channels (Mono/Stereo/etc), which we received as
     * an argument.
     *
     * We also set the sample rate, that is, the number of samples per
     * second. We also received it as an argument.
     *
     * Then we specify the `BitsPerSample' property, and it's important to note
     * that it's the size in bits of a sample in a single channel.
     *
     * The next property, `BlockAlign', specifies the size in bytes (not bits)
     * of a sample in all channels (not on a single one).
     *
     * Finally, we set the `ByteRate', that is, the size in bytes of all samples
     * in a second.
     */
    memcpy(h->Subchunk1ID, "fmt ", 4);
    h->Subchunk1Size = 16;
    h->AudioFormat   = PCM;
    h->NumChannels   = num_channels;
    h->SampleRate    = sample_rate;
    h->BitsPerSample = 16;
    h->BlockAlign    = h->NumChannels * (h->BitsPerSample / 8);
    h->ByteRate      = h->SampleRate * h->BlockAlign;

    /*
     * We start with the "data" sub-chunk, which indicates the size of the sound
     * information. It also contains the data itself, but we won't set it in
     * this function.
     *
     * After the sub-chunk ID, we calculate the total number of samples from the
     * audio length in seconds, which we received as an argument. We multiply
     * the total number of samples by the number of bytes in a sample (in all
     * channels), and we obtain the `Subchunk2Size' property.
     */
    memcpy(h->Subchunk2ID, "data", 4);
    const int num_samples = h->SampleRate * audio_len_secs;
    h->Subchunk2Size      = num_samples * h->BlockAlign;

    /*
     * Finally, we set the `ChunkSize' property from the "RIFF" chunk
     * descriptor, the one we missed above. It's the size in bytes of the whole
     * buffer, including this header and the actual sample data (which we
     * haven't written yet).
     */
    h->ChunkSize = h->Subchunk2Size + sizeof(WavHeader);
}

/*
 * Generate a sample from the specified frequency, amplitude and sample rate.
 * Uses a sinusoidal function. For more information, see:
 *   -  https://www.math.net/sinusoidal
 *   -  https://www.desmos.com/calculator/fuv5xs95i5
 */
static inline uint16_t generate_sample(int sample_number, double freq,
                                       int amplitude, int sample_rate) {
    return (uint16_t)(cos((2 * M_PI * freq * sample_number) / sample_rate) *
                      amplitude);
}

/*
 * Fill the body of a WAV file with some test samples.
 */
static void fill_wav_body(WavHeader* header, uint32_t* data,
                          size_t num_samples) {
    const int wave_amplitude = 1000;
    const double note_freq   = 256.0;

    /*
     * Populate both channels of `data' with `num_samples' notes of frequency
     * `note_freq'.
     *
     * The left samples will occupy the 16 most significant bits (left), and the
     * right samples will occupy the 16 least significant bits (right).
     */
    for (size_t i = 0; i < num_samples; i++) {
        const uint16_t left_sample =
          generate_sample(i, note_freq + 100, wave_amplitude,
                          header->SampleRate);
        data[i] = left_sample << 16;

        const uint16_t right_sample =
          generate_sample(i, note_freq - 100, wave_amplitude,
                          header->SampleRate);
        data[i] |= right_sample;
    }
}

int main(void) {
    WavHeader* header = malloc(sizeof(WavHeader));

    /*
     * Initialize the WAV header, specifying the number of channels, the samples
     * per second, and the audio length in seconds.
     */
    const enum ENumChannels num_channels = STEREO;
    const int sample_rate                = 8000;
    const int audio_length               = 10;
    init_wav_header(header, num_channels, sample_rate, audio_length);

    /*
     * Each element should be `header.BlockAlign' (2 bytes * 2 channels in our
     * case).
     */
    const size_t num_samples = header->SampleRate * audio_length;
    uint32_t* data           = malloc(num_samples * sizeof(uint32_t));
    fill_wav_body(header, data, num_samples);

    /*
     * Write the header and data to the output file.
     */
    FILE* fp = fopen("output.wav", "wb");
    fwrite(header, 1, sizeof(WavHeader), fp);
    fwrite(data, sizeof(uint32_t), num_samples, fp);
    fclose(fp);

    free(data);
    free(header);
    return 0;
}
