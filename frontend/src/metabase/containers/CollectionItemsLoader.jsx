/* @flow */
import React from "react";
import EntityObjectLoader from "metabase/entities/containers/EntityObjectLoader";

type Props = {
  collectionId: number,
  children: () => void,
};

const CollectionItemsLoader = ({ collectionId, children, ...props }: Props) => (
  <EntityObjectLoader
    {...props}
    entityType="collections"
    entityId={collectionId}
    children={({ object }) =>
      object &&
      children({
        collection: object,
        dashboards: object.dashboards,
        cards: object.cards,
        pulses: object.pulses,
        allItems: [].concat(
          object.dashboards.map(d => ({ ...d, type: "dashboard" })),
          object.cards.map(c => ({ ...c, type: "card" })),
          object.pulses.map(p => ({ ...p, type: "pulse" })),
        ),
        empty:
          object.dashboards.length === 0 &&
          object.cards.length === 0 &&
          object.pulses.length,
      })
    }
  />
);

export default CollectionItemsLoader;
