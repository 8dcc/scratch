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
 */

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <math.h> /* cosf, M_PI */

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

/*----------------------------------------------------------------------------*/

/*
 * Initialize a `WavHeader' structure with the specified parameters.
 *
 * Credits: http://soundfile.sapp.org/doc/WaveFormat/
 */
static void init_wav_header(WavHeader* h,
                            enum ENumChannels num_channels,
                            int sample_rate,
                            int audio_len_secs) {
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
     * second. We also received it as an argument. This is independent of
     * the number of channels.
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
    const int total_samples = h->SampleRate * audio_len_secs;
    h->Subchunk2Size        = total_samples * h->BlockAlign;

    /*
     * Finally, we set the `ChunkSize' property from the "RIFF" chunk
     * descriptor, the one we missed above. It's the size in bytes of the whole
     * buffer, including this header and the actual sample data (which we
     * haven't written yet).
     */
    h->ChunkSize = h->Subchunk2Size + sizeof(WavHeader);
}

/*
 * Allocate the necessary samples for the specified WAV header.
 *
 * Remember that `header.BlockAlign' is the size of a sample in all channels. We
 * also calculate the total samples in the file from the samples per second and
 * the audio length. We multiply those two numbers to obtain the number of bytes
 * the caller needs.
 */
static void* allocate_wav_data(const WavHeader* header, int audio_len_secs) {
    const size_t total_samples    = header->SampleRate * audio_len_secs;
    const size_t bytes_per_sample = header->BlockAlign;
    return malloc(total_samples * bytes_per_sample);
}

/*
 * Generate a sample from the specified frequency, amplitude and sample rate.
 * For more information, see:
 *   -  https://www.math.net/sinusoidal
 *   -  https://www.desmos.com/calculator/fuv5xs95i5
 */
static inline uint16_t generate_sample(int sample_number,
                                       double freq,
                                       int amplitude,
                                       int sample_rate) {
    /*
     * The period of the sinusoidal wave determines how wide is a full cycle or
     * oscillation. The base period would be used for sinusoidal waves of 1Hz,
     * where a cycle is performed each unit in the X axis. That based period is
     * multiplied by the frequency to obtain the actual period of the desired
     * wave.
     */
    const double base_period = 2 * M_PI;
    const double period      = base_period * freq;

    /*
     * Given the period of the target wave, the phase angle at the specific
     * sample is obtained.
     */
    const double phase_angle = (period * sample_number) / sample_rate;

    /*
     * The cosine of the phase angle can be used to obtain the Y coordinate of
     * the wave at that specific point. This can be multiplied by the amplitude
     * to return the final sample value.
     */
    return (uint16_t)(cos(phase_angle) * amplitude);
}

/*
 * Fill the data of a WAV file with samples of the specified amplitude and
 * frequency, for a specified number of seconds.
 *
 * In this case, it's assumed that `header.BitsPerSample' is 16, since it's
 * hard-coded in `init_wav_header'.
 */
static void fill_data_mono(const WavHeader* header,
                           uint16_t* data,
                           int seconds,
                           int amplitude,
                           double freq) {
    assert(header->BitsPerSample == 16);
    assert(header->NumChannels == MONO);

    const size_t total_samples = header->SampleRate * seconds;
    for (size_t i = 0; i < total_samples; i++)
        data[i] = generate_sample(i, freq, amplitude, header->SampleRate);
}

/*
 * Fill the data of a WAV file with samples of the specified amplitude and
 * frequency, for a specified number of seconds.
 *
 * In this case, it's assumed that `header.BitsPerSample' is 16, since it's
 * hard-coded in `init_wav_header'. Each sample will be 32 bits, the left
 * channel sample will occupy the first 16 bits, and the right channel sample
 * will occupy the least significant 16 bits.
 */
static void fill_data_stereo(const WavHeader* header,
                             uint16_t* data,
                             int seconds,
                             int amplitude,
                             double freq) {
    assert(header->BitsPerSample == 16);
    assert(header->NumChannels == STEREO);

    const size_t total_samples = header->SampleRate * seconds;
    for (size_t i = 0; i < total_samples; i++) {
        const uint16_t left_sample =
          generate_sample(i, freq + 100, amplitude, header->SampleRate);
        *data++ = left_sample;

        const uint16_t right_sample =
          generate_sample(i, freq - 100, amplitude, header->SampleRate);
        *data++ = right_sample;
    }
}

/*----------------------------------------------------------------------------*/

int main(void) {
    /*
     * Allocate the WAV header structure.
     */
    WavHeader* header = malloc(sizeof(WavHeader));
    if (header == NULL) {
        fprintf(stderr, "Could not allocate %zu bytes.\n", sizeof(WavHeader));
        return 1;
    }

    /*
     * Initialize the WAV header, specifying the number of channels, the samples
     * per second, and the audio length in seconds.
     */
    const enum ENumChannels num_channels = MONO;
    const int sample_rate                = 8000;
    const int audio_len_secs             = 10;
    init_wav_header(header, num_channels, sample_rate, audio_len_secs);

    /*
     * Allocate the actual WAV data array, which will contain all the audio
     * samples.
     */
    uint16_t* data = allocate_wav_data(header, audio_len_secs);
    if (data == NULL) {
        fprintf(stderr, "Could not WAV data array.\n");
        free(header);
        return 1;
    }

    /*
     * Allocate and fill the WAV data with a static tone. Set the wave amplitude
     * and the note frequency here. In the stereo version, each channel will
     * have a slightly different frequency.
     */
    const int amplitude = 1000;
    const double freq   = 256.0;
    switch (header->NumChannels) {
        case MONO:
            fill_data_mono(header, data, audio_len_secs, amplitude, freq);
            break;

        case STEREO:
            fill_data_stereo(header, data, audio_len_secs, amplitude, freq);
            break;

        default:
            fprintf(stderr,
                    "Invalid number of channels: %d\n",
                    header->NumChannels);
            return 1;
    }

    /*
     * Write the header and data to the output file.
     * FIXME: Test in big-endian systems.
     */
    FILE* fp = fopen("output.wav", "wb");
    fwrite(header, 1, sizeof(WavHeader), fp);
    fwrite(data, header->BlockAlign, header->SampleRate * audio_len_secs, fp);
    fclose(fp);

    free(data);
    free(header);
    return 0;
}
