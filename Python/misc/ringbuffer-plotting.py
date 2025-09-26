#!/usr/bin/env python3

class RingBuffer:
    def __init__(self, size):
        self.size = size  # Fixed number of elements at all times
        self.buf = []
        for _ in range(self.size):
            self.buf.append(None)

    def getSize(self):
        return self.size

    def getBuffer(self):
        return self.buf

    def append(self, element):
        self.buf.pop(0)  # Discard first element, shifting array to the left
        self.buf.append(element)

def main():
    ring_buffer = RingBuffer(10)
    for i in range(ring_buffer.getSize()):
        ring_buffer.append(i + 100)
    print(f"Before: {ring_buffer.getBuffer()}")

    ring_buffer.append("a")
    ring_buffer.append("b")
    ring_buffer.append("c")
    ring_buffer.append("d")
    print(f"After:  {ring_buffer.getBuffer()}")

if __name__ == "__main__":
    main()
