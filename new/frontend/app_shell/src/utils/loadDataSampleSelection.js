function sortZoneSamples(samples, sampleOrder) {
  const order = Array.isArray(sampleOrder) ? sampleOrder : [];

  if (!order.length) {
    return [...samples];
  }

  const rank = new Map(order.map((sample, index) => [sample, index]));
  return [...samples].sort((left, right) => {
    const leftRank = rank.has(left) ? rank.get(left) : Number.MAX_SAFE_INTEGER;
    const rightRank = rank.has(right) ? rank.get(right) : Number.MAX_SAFE_INTEGER;
    return leftRank - rightRank;
  });
}

export function normalizeSelectedSamples(selectedSamples, allowedSamples) {
  const allowed = new Set(Array.isArray(allowedSamples) ? allowedSamples : []);
  const normalized = [];

  (Array.isArray(selectedSamples) ? selectedSamples : []).forEach((sample) => {
    if (allowed.has(sample) && !normalized.includes(sample)) {
      normalized.push(sample);
    }
  });

  return normalized;
}

export function nextSelectedSamples(selectedSamples, sampleName, additive = false) {
  const current = Array.isArray(selectedSamples) ? selectedSamples : [];

  if (!sampleName) {
    return additive ? current : [];
  }

  if (additive) {
    return current.includes(sampleName)
      ? current.filter((sample) => sample !== sampleName)
      : [...current, sampleName];
  }

  return [sampleName];
}

export function dragSelectionForSample(selectedSamples, sampleName) {
  const current = Array.isArray(selectedSamples) ? selectedSamples : [];

  if (sampleName && current.includes(sampleName)) {
    return current;
  }

  return sampleName ? [sampleName] : [];
}

export function buildDragPreviewLabel(movedSamples) {
  const count = Array.isArray(movedSamples) ? movedSamples.filter(Boolean).length : 0;
  return count === 1 ? "1 sample" : `${count} samples`;
}

export function moveSamplesToZone(assignments, movedSamples, targetZone, sampleOrder = []) {
  const currentAssignments = assignments || { unassigned: [], rna: [], ribo: [] };
  const nextMovedSamples = Array.isArray(movedSamples) ? movedSamples.filter(Boolean) : [];

  if (!nextMovedSamples.length || !["unassigned", "rna", "ribo"].includes(targetZone)) {
    return {
      assignments: currentAssignments,
      selectedSamples: nextMovedSamples
    };
  }

  const movedSet = new Set(nextMovedSamples);
  const nextAssignments = {
    unassigned: (currentAssignments.unassigned || []).filter((sample) => !movedSet.has(sample)),
    rna: (currentAssignments.rna || []).filter((sample) => !movedSet.has(sample)),
    ribo: (currentAssignments.ribo || []).filter((sample) => !movedSet.has(sample))
  };

  nextAssignments[targetZone] = sortZoneSamples([...nextAssignments[targetZone], ...nextMovedSamples], sampleOrder);

  return {
    assignments: {
      unassigned: sortZoneSamples(nextAssignments.unassigned, sampleOrder),
      rna: sortZoneSamples(nextAssignments.rna, sampleOrder),
      ribo: sortZoneSamples(nextAssignments.ribo, sampleOrder)
    },
    selectedSamples: nextMovedSamples
  };
}
