#!/usr/bin/env sh

# TODO: this stops working if the default sink changes. Perhaps it's possible to
# 1. start a subprocess with this process as argument
# 2. trigger a SIGHUP and restart this process when 
# or create a more general purpose tool for this, there's a nascent pipewire-rs for example

default_audio_sink_node_name="$(pw-dump | jq ".[] | select(.type == \"PipeWire:Interface:Metadata\").metadata[] | select(.key == \"default.audio.sink\") | .value.name")"

exec -- stdbuf -oL pw-dump --monitor | stdbuf -oL jq -c -M "(.[] | select(.type == \"PipeWire:Interface:Node\") | select(.info.props.\"node.name\" == $default_audio_sink_node_name)) as \$default_audio_sink_node | (.[] | select(.type == \"PipeWire:Interface:Device\" and .id == \$default_audio_sink_node.info.props.\"device.id\")) as \$default_audio_sink_device | (\$default_audio_sink_device.info.params.Route[] | select(.direction == \"Output\")) as \$route | { device: \$default_audio_sink_device, node: \$default_audio_sink_node, route: \$route }"
