package main

import (
	"fmt"
	"log"
	"os"
	"strings"

	"tailscale.com/atomicfile"
)

func main() {
	zone := os.Args[1]
	output := os.Args[2]

	ts := &Tailscale{}
	ts.zone = zone
	ts.signal = make(chan bool)

	if err := ts.start(); err != nil {
		log.Println(err)
		return
	}

	for range ts.signal {
		rpz := ts.generateRPZ()
		atomicfile.WriteFile(output, []byte(rpz), 0644)
	}
}

func (t *Tailscale) generateRPZ() string {
	var builder strings.Builder

	for host, dns := range t.entries {
		for record, values := range dns {
			for _, v := range values {
				rpz := fmt.Sprintf("%s.%s	%s	%s\n", host, t.zone, record, v)
				if _, err := builder.WriteString(rpz); err != nil {
					log.Println(err)
				}
			}
		}
	}

	return builder.String()
}
