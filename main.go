package main

import (
	"flag"
	"fmt"
	"log"
	"math/rand"
	"regexp"
	"strings"

	"tailscale.com/atomicfile"
)

// Tailscale suffixes ephemeral with -N
// Central names are suffixed with -central such that the node and any other
// ephermal node names derived from it are centralized to the prefix
// For example node-central, node-central-1, and node-central-2 will be aliased
// records to node
var EPHERMAL_REGEX *regexp.Regexp = regexp.MustCompile("^([^-]+.*)-central(?:-[0-9]+){0,1}$")

const SOA_RECORD string = `
$TTL 300
@ SOA localhost. root.localhost. 1721221718 43200 3600 259200 300
  NS  localhost.

`

var central = flag.Bool("central", false, "Provide records to a central alias")
var shuffle = flag.Bool("shuffle", false, "Shuffle order of hosts every update")

func main() {
	flag.Parse()
	if len(flag.Args()) < 2 {
		log.Printf("tailscale-rpz [--central] [--shuffle] <zone> <rpz-output-file>\n")
		return
	}

	zone := flag.Arg(0)
	output := flag.Arg(1)

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

	// Shuffle the keys randomly
	hosts := make([]string, 0, len(t.entries))
	for host := range t.entries {
		hosts = append(hosts, host)
	}
	if *shuffle {
		rand.Shuffle(len(hosts), func(i, j int) {
			hosts[i], hosts[j] = hosts[j], hosts[i]
		})
	}

	for _, host := range hosts {
		dns := t.entries[host]
		for record, values := range dns {
			for _, v := range values {
				if splits := EPHERMAL_REGEX.FindStringSubmatch(host); *central && splits != nil {
					centralAlias := splits[1]

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
