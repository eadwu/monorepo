package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"

	"tailscale.com/atomicfile"
)

// Tailscale suffixes ephemeral with -N
// Match everything behind (.*)-N for the central domain for the rest to load balance
// To make it easier, matching is relaxed to any number of `-` so the name can be
// "nodename-" and ephemeral instances would be "nodename--1", "nodenname--2"
// and the central instance is "nodename"
var EPHERMAL_REGEX *regexp.Regexp = regexp.MustCompile(`([^-]+)-*-[0-9]+`)

const SOA_RECORD string = `
$TTL 300
@ SOA localhost. root.localhost. 1721221718 43200 3600 259200 300
  NS  localhost.

`

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

	if _, err := builder.WriteString(SOA_RECORD); err != nil {
		log.Println(err)
	}

	for host, dns := range t.entries {
		for record, values := range dns {
			for _, v := range values {
				if EPHERMAL_REGEX.MatchString(host) {
					splits := strings.Split(host, "-")
					centralAlias := splits[0]

					rpz := fmt.Sprintf("%s.%s	%s	%s\n", centralAlias, t.zone, record, v)
					if _, err := builder.WriteString(rpz); err != nil {
						log.Println(err)
					}
				}

				rpz := fmt.Sprintf("%s.%s	%s	%s\n", host, t.zone, record, v)
				if _, err := builder.WriteString(rpz); err != nil {
					log.Println(err)
				}
			}
		}
	}

	return builder.String()
}
