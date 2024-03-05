use futures_util::StreamExt;

#[derive(Clone)]
pub struct Mercenary {
    identifier: uuid::Uuid,
    client: async_nats::Client,
}

struct MercenaryChannel {
    topic: String,
    queue_group: String,
}

impl Mercenary {
    pub fn new(nc: async_nats::Client) -> Mercenary {
        let id = uuid::Uuid::new_v4();
        Mercenary {
            identifier: id,
            client: nc,
        }
    }

    fn identifier(&self) -> String {
        self.identifier.to_string()
    }

    fn nats_client(&self) -> &async_nats::Client {
        &self.client
    }

    fn topic(&self) -> String {
        format!("{}.{}", guild::GUILD_MERCENARY_TOPIC, &self.identifier)
    }

    pub async fn routine(&self) -> Result<(), async_nats::Error> {
        tracing::info!(
            "Mercenary `{}` has been recruited for operations",
            &self.identifier
        );

        let communication_channels = [
            // Quest Board (broadcast)
            MercenaryChannel::new(
                guild::GUILD_QUEST_BOARD_TOPIC,
                guild::GUILD_ALL_MERCENARY_QUEUE_GROUP,
            ),
            // Direct Communication (unicast)
            MercenaryChannel::new(self.topic(), self.identifier()),
        ];

        let mut handles = Vec::with_capacity(communication_channels.len());
        for channel in communication_channels {
            tracing::debug!(
                "Mercenary `{}` is listening on topic `{}` (group `{}`)",
                &self.identifier,
                &channel.topic,
                &channel.queue_group
            );
            handles.push(tokio::spawn(
                self.clone().handler(channel.topic, channel.queue_group),
            ))
        }
        for handle in handles {
            handle.await??
        }
        Ok(())
    }

    async fn handler<T: Into<String>>(
        self,
        topic: T,
        queue_group: T,
    ) -> Result<(), async_nats::Error> {
        let nats_client = self.nats_client();
        let mut subscription = nats_client
            .queue_subscribe(topic.into(), queue_group.into())
            .await?;

        while let Some(quest_msg) = subscription.next().await {
            let quest = serde_json::from_slice::<guild::GuildQuest>(&quest_msg.payload)?;
            let quest_identifier = quest.identifier;
            tracing::info!(
                "Mercenary `{}` accepted quest `{}`",
                &self.identifier,
                &quest_identifier
            );

            if let Some(reply_subject) = quest_msg.reply {
                tracing::debug!("Relaying status update of `{}` to `{}`", &quest_identifier, &reply_subject);

                let response =
                    guild::GuildQuestAcknowledgement::accept(quest_identifier, self.identifier());
                let payload = serde_json::to_string(&response)?;
                nats_client.publish(reply_subject, payload.into()).await?;
            }
        }
        Ok(())
    }
}

impl MercenaryChannel {
    fn new<T: Into<String>>(topic: T, queue_group: T) -> MercenaryChannel {
        MercenaryChannel {
            topic: topic.into(),
            queue_group: queue_group.into(),
        }
    }
}
